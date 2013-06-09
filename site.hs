--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Hakyll
import           Hakyll.Web.Tags

import           Control.Applicative              ( (<$>))
import           Control.Monad                    (forM)
import           Control.Arrow                    (first, second)

import           Data.Ord
import           Data.List                        (sortBy, groupBy)
import           Data.Map                         (lookup)  
import           Data.Monoid                      (mappend, mconcat)
import           Data.Function                    (on)

import           Data.Time.Format                 (formatTime)

import           System.Locale                    (defaultTimeLocale)

import           Text.Pandoc.Options

import           Text.Regex.Posix                 hiding (match)

import           Plugins.Filters
import           Plugins.LogarithmicTagCloud


--------------------------------------------------------------------------------

pandocOptions :: WriterOptions
pandocOptions = defaultHakyllWriterOptions
    { writerHTMLMathMethod = MathJax ""
    }

hakyllConf :: Configuration
hakyllConf = defaultConfiguration {
  deployCommand =
     "rsync -ave ssh _site/ " ++
     "xinitrc@corvus.uberspace.de:html/"
  }

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "xinitrc.de"
    , feedDescription = "xinitrc.de Article Feed"
    , feedAuthorName  = "Martin Hilscher"
    , feedAuthorEmail = "mail@xinitrc.de"
    , feedRoot        = "https://xinitrc.de"
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
            "talks/**"  .||. "bootstrap/**" .||. "scripts/**" .||.
            "adaptive-images.php")  $ do
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
                
    match "htaccess" $ do
        route $ constRoute ".htaccess"
        compile copyFileCompiler
    
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
            let indexCtx = field "posts" (\_ -> postList $ fmap (take 3) . getTalks) `mappend` 
                            (taggedPostCtx tags)

            getResourceBody
                >>= applyKeywords
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/blog.html" (taggedPostCtx tags)
              
    match "archive.html" $ do
        route idRoute
        compile $ do
            let indexCtx = field "posts" ( \_ -> postListByMonth tags "posts/*" recentFirst)
            getResourceBody
                >>= applyKeywords
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/blog.html" (taggedPostCtx tags)
          
    match "talk-archive.html" $ do
        route idRoute
        compile $ do
            let indexCtx = field "posts" ( \_ -> postListByMonth tags "posts/*" getTalks)
            getResourceBody
                >>= applyKeywords
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/blog.html" (taggedPostCtx tags)

    match "index.html" $ do
        route idRoute
        compile $ do
            let indexCtx = (field "posts" $ \_ -> postList $ fmap (take 3) . recentFirst) `mappend` 
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
  return $ map snd $ reverse $ sortBy (comparing fst) $ map snd $ filter fst itemsWithTime
  
isTalk :: MonadMetadata m => Identifier -> m Bool 
isTalk id' = do
    metadata <- getMetadata id'
    let typ = Data.Map.lookup "type" metadata
    return (typ == Just "talk")

--------------------------------------------------------------------------------

tagCloudCtx :: Tags -> Context String
tagCloudCtx tags = field "tagcloud" $ \item -> rendered 
  where rendered = renderLogTagCloud 85.0 165.0 "%" tags

taggedPostCtx :: Tags -> Context String
taggedPostCtx tags = mconcat [(tagsField "tags" tags), (tagCloudCtx tags), postCtx]

postCtx :: Context String
postCtx = mconcat [dateField "date" "%Y %b %d" , defaultContext]

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
                 -> ([Item String] -> Compiler [Item String])
                 -> Compiler String
postListByMonth tags pattern filterFun = do
    posts   <- bucketMonth =<< filterFun =<< loadAll pattern
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
          
