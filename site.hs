--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Hakyll
import           Hakyll.Web.Tags
import           Hakyll.Web.Template.Context 

import           Control.Applicative
import           Control.Monad
import           Control.Arrow
import           System.Locale                    (defaultTimeLocale)

import           Data.Ord
import           Data.List                        (intercalate, intersperse, sortBy, groupBy)
import           Data.Map                         (lookup)  
import           Data.Monoid                      (mappend, mconcat)

import           Data.Time.Format
import           Data.Function

import           Filters

import qualified Text.Blaze.Html5                 as H
import qualified Text.Blaze.Html5.Attributes      as A
import           Text.Blaze.Html                  (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String  (renderHtml)

import           Hakyll.Core.Compiler
import           Hakyll.Core.Dependencies
import           Hakyll.Core.Identifier
import           Hakyll.Core.Identifier.Pattern
import           Hakyll.Core.Item
import           Hakyll.Core.Metadata
import           Hakyll.Core.Rules
import           Hakyll.Core.Util.String
import           Hakyll.Web.Template.Context
import           Hakyll.Web.Html
import           Hakyll.Web.Pandoc
import           Text.Pandoc.Options


import           Text.Regex.Posix hiding (match)


--------------------------------------------------------------------------------

hakyllConf :: Configuration
hakyllConf = defaultConfiguration {
  deployCommand =
     "rsync -ave ssh _site/ " ++
     "xinitrc@corvus.uberspace.de:html/"
  }

--------------------------------------------------------------------------------

main :: IO ()
main = hakyllWith hakyllConf $ do

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    tagsRules tags $ \tag pattern -> do
           route idRoute
           compile $ do
               posts <- constField "posts" <$> postLst pattern "templates/tag-item.html" (taggedPostCtx tags) recentFirst
    
               makeItem ""
                   >>= loadAndApplyTemplate "templates/tagpage.html" (posts `mappend` (taggedPostCtx tags))
  
    match ("images/*"   .||. "favicon.ico"  .||. "assets/**"  .||. 
            "talks/**"  .||. "bootstrap/**" .||. "data/**"    .||. 
            "scripts/**")  $ do
        route   idRoute
        compile copyFileCompiler
        
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match ("facts.html" .||. "contact.html" )$ do
        route idRoute
        compile $ do
            getResourceBody
                >>= applyKeywords
                >>= loadAndApplyTemplate "templates/main.html" (taggedPostCtx tags)
    
    match "posts/*" $ do
        route $ dateRoute
        compile $ do
            pandocCompilerWith defaultHakyllReaderOptions pandocOptions
            >>= applyKeywords
            >>= saveSnapshot "teaser"
            >>= loadAndApplyTemplate "templates/post.html" (taggedPostCtx tags)
            >>= loadAndApplyTemplate "templates/blog.html" (taggedPostCtx tags)

    match "talks.html" $ do 
        route idRoute
        compile $ do
            let indexCtx = field "posts" (\_ -> postList getTalks) `mappend` (taggedPostCtx tags)

            getResourceBody
                >>= applyKeywords
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/blog.html" (taggedPostCtx tags)
              
    match "archives.html" $ do
        route idRoute
        compile $ do
            let indexCtx = field "posts" ( \_ -> postListByMonth tags "posts/*")
            getResourceBody
                >>= applyKeywords
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/blog.html" (taggedPostCtx tags)
          
    match "index.html" $ do
        route idRoute
        compile $ do
            let indexCtx = (field "posts" $ \_ ->
                                postList $ fmap (take 3) . recentFirst) `mappend`
                            (taggedPostCtx tags)

            getResourceBody
                >>= applyKeywords
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/blog.html" (taggedPostCtx tags)
                
                
    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend` bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "posts/*" "teaser"
            renderAtom myFeedConfiguration feedCtx posts

    match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
applyFilter strfilter str = return $ (fmap $ strfilter) str

-- preFilters :: String -> String
--------------------------------------------------------------------------------
dateRoute :: Routes
dateRoute = gsubRoute "posts/" (const "") `composeRoutes` 
              gsubRoute "pages/" (const "") `composeRoutes`
                gsubRoute "[0-9]{4}-[0-9]{2}-[0-9]{2}-" (map replaceChars) `composeRoutes` 
                 setExtension "html" 
            where
              replaceChars c | c == '-' || c == '_' = '/'
                             | otherwise = c

--------------------------------------------------------------------------------

getTalks :: [Item String] -> Compiler [Item String]
getTalks items = do
  itemsWithTime <- forM items $ \item -> do
    talk <- isTalk $ itemIdentifier item
    utc <- getItemUTC defaultTimeLocale $ itemIdentifier item
    return (talk, (utc,item))
  -- we return a sorted item list
  return $ map snd $ reverse $ sortBy (comparing fst) $ map snd $ filter fst itemsWithTime
  
isTalk :: MonadMetadata m => Identifier -> m Bool 
isTalk id' = do
    metadata <- getMetadata id'
    let typ = Data.Map.lookup "type" metadata
    return (typ == Just "talk")


--------------------------------------------------------------------------------

tagCloudCtx :: Tags -> Context String
tagCloudCtx tags = field "tagcloud" $ \item -> rendered 
  where rendered = renderLogTagCloud 85.0 165.0 tags

taggedPostCtx :: Tags -> Context String
taggedPostCtx tags = mconcat [(tagsField "tags" tags), (tagCloudCtx tags), postCtx]

postCtx :: Context String
postCtx = mconcat [dateField "date" "%Y %b %d" , defaultContext]

renderLogTagCloud :: Double
               -- ^ Smallest font size, in percent
               -> Double
               -- ^ Biggest font size, in percent
               -> Tags
               -- ^ Input tags
               -> Compiler String
               -- ^ Rendered cloud
renderLogTagCloud minSize maxSize = renderTags makeLink (intercalate " ")
  where
    makeLink tag url count min' max' = renderHtml $
        H.a ! A.style (toValue $ "font-size: " ++ size count min' max')
            ! A.href (toValue url)
            $ toHtml tag

    -- Show the relative size of one 'count' in percent
    size count min' max' =
        let diff = (log (fromIntegral max') - log (fromIntegral min'))
            relative = (log (fromIntegral count) - log (fromIntegral min')) / diff
            size' = floor $ minSize + relative * (maxSize - minSize)
        in show (size' :: Int) ++ "%"

--------------------------------------------------------------------------------

pandocOptions :: WriterOptions
pandocOptions = defaultHakyllWriterOptions
    { writerHTMLMathMethod = MathJax ""
    }

--------------------------------------------------------------------------------

postLst :: Pattern -> Identifier -> Context String -> ([Item String] -> Compiler [Item String]) -> Compiler String
postLst pattern template context sortFilter = do
    posts   <- sortFilter =<< loadAll pattern
    itemTpl <- loadBody template
    list    <- applyTemplateList itemTpl ((teaserField "teaser" "teaser") `mappend` context) posts
    return list

postList :: ([Item String] -> Compiler [Item String]) -> Compiler String
postList = postLst "posts/*" "templates/post-item.html" postCtx

postListByMonth :: Tags
                 -> Pattern
                 -> Compiler String
postListByMonth tags pattern = do
    posts   <- bucketMonth =<< recentFirst =<< loadAll pattern
    itemTpl <- loadBody "templates/month-item.html"
    monthTpl <- loadBody "templates/month.html"
    let mkSection ((yr, _, mth), ps) =
            applyTemplateList itemTpl (taggedPostCtx tags `mappend` (dateField "day" "%d")) ps >>=
            makeItem                                          >>=
            applyTemplate monthTpl (monthContext yr mth)
    concatMap itemBody <$> mapM mkSection posts
  where
    bucketMonth posts =
        reverse . map (second (map snd)) . buckets fst <$>
        mapM tagWithMonth posts
    tagWithMonth p = do
        utcTime <- getItemUTC timeLocale (itemIdentifier p)
        return ((formatTime timeLocale "%Y" utcTime,formatTime timeLocale "%m" utcTime, formatTime timeLocale "%b" utcTime ), p)
    timeLocale = defaultTimeLocale
 
monthContext :: String -> String -> Context String
monthContext year month = mconcat
    [ constField "year"  year
    , constField "month" month
    , bodyField  "postsByMonth"
    ]
 
buckets :: Ord b => (a -> b) -> [a] -> [ (b,[a]) ]
buckets f = map (first head . unzip)
          . groupBy ((==) `on` fst)
          . sortBy (compare `on` fst)
          . map (\x -> (f x, x))

--------------------------------------------------------------------------------
          
myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "xinitrc.de"
    , feedDescription = "xinitrc.de Article Feed"
    , feedAuthorName  = "Martin Hilscher"
    , feedAuthorEmail = "mail@xinitrc.de"
    , feedRoot        = "https://xinitrc.de"
    }

