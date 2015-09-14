--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Hakyll

import           Control.Applicative              ((<$>))
import           Control.Monad                    (filterM, liftM, (>=>), (<=<))
import           Control.Arrow                    (first, second)

import qualified Data.Set as S
import           Data.List                        (sortBy, groupBy)
import           Data.Map                         (lookup)  
import           Data.Monoid                      (mappend, mconcat)
import           Data.Function                    (on)

import           Data.Time.Format                 (formatTime, defaultTimeLocale)

import           Plugins.Filters (applyKeywords, aplKeywords)
import           Plugins.LogarithmicTagCloud (renderLogTagCloud)

import           Text.Pandoc.Options


--------------------------------------------------------------------------------

host :: String
host = "https://xinitrc.de"

pandocWriterOptions :: WriterOptions
pandocWriterOptions = defaultHakyllWriterOptions 
                      { writerHTMLMathMethod = MathML Nothing -- MathJax ""
                      , writerHtml5 = True
                      , writerSectionDivs = True
                      , writerReferenceLinks = True
                      }

pandocReaderOptions :: ReaderOptions
pandocReaderOptions = defaultHakyllReaderOptions 
                      { readerExtensions = myPandocExtensions
                      }

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
                      { feedTitle       = "xinitrc.de"
                      , feedDescription = "xinitrc.de Article Feed"
                      , feedAuthorName  = "Martin Hilscher"
                      , feedAuthorEmail = "mail@xinitrc.de"
                      , feedRoot        = host
                      }

feedConfiguration :: String -> FeedConfiguration
feedConfiguration title = FeedConfiguration
                          { feedTitle = title
                          , feedDescription = "xinitrc.de Tag Configuration"
                          , feedAuthorName = "Martin Hilscher"
                          , feedAuthorEmail = "mail@xinitrc.de"
                          , feedRoot = host
                          }

dontIgnoreHtaccess :: String -> Bool
dontIgnoreHtaccess ".htaccess" = False
dontIgnoreHtaccess path        = ignoreFile defaultConfiguration path

hakyllConf :: Configuration
hakyllConf = defaultConfiguration 
             {
               deployCommand = "rsync -ave ssh _site/ xinitrc@corvus.uberspace.de:html/"
             , ignoreFile = dontIgnoreHtaccess
             }

myPandocExtensions :: S.Set Extension
myPandocExtensions = S.fromList
                     [ Ext_footnotes
                     , Ext_inline_notes
                     , Ext_pandoc_title_block
                     , Ext_table_captions
                     , Ext_implicit_figures
                     , Ext_simple_tables
                     , Ext_multiline_tables
                     , Ext_grid_tables
                     , Ext_pipe_tables
                     , Ext_citations
                     , Ext_raw_tex
                     , Ext_raw_html
                     , Ext_tex_math_dollars
                     , Ext_tex_math_single_backslash
                     , Ext_latex_macros
                     , Ext_fenced_code_blocks
                     , Ext_fenced_code_attributes
                     , Ext_backtick_code_blocks
                     , Ext_inline_code_attributes
                     , Ext_markdown_in_html_blocks
                     , Ext_escaped_line_breaks
                     , Ext_fancy_lists
                     , Ext_startnum
                     , Ext_definition_lists
                     , Ext_example_lists
                     , Ext_all_symbols_escapable
                     , Ext_intraword_underscores
                     , Ext_blank_before_blockquote
                     , Ext_blank_before_header
                     , Ext_strikeout
                     , Ext_superscript
                     , Ext_subscript
                     , Ext_auto_identifiers
                     , Ext_header_attributes
                     , Ext_implicit_header_references
                     , Ext_line_blocks
                     ]

--------------------------------------------------------------------------------

main :: IO ()
main = hakyllWith hakyllConf $ do
    tags <- buildTags "blog/*" (fromCapture "tags/*.html")

    tagsRules tags $ \tag pattern -> do
           let title = "Posts tagged " ++ tag
           route $ gsubRoute " " (const "-")
           compile $ do
               posts <- constField "posts" <$> postLst pattern "templates/tag-item.html" (taggedPostCtx tags) recentFirst
               makeItem ""
                   >>= loadAndApplyTemplate "templates/tagpage.html" (posts `mappend` constField "tag" tag `mappend` taggedPostCtx tags)
                   >>= loadAndApplyTemplate "templates/main.html" (posts `mappend` constField "tag" tag `mappend` taggedPostCtx tags)
           version "rss" $ do
            route   $ gsubRoute " " (const "-") `composeRoutes` setExtension "xml"
            compile $ loadAllSnapshots pattern "teaser"
                >>= fmap (take 10) . recentFirst
                >>= renderAtom (feedConfiguration title) feedContext

    match "static/**" $ do
        route   $ gsubRoute "static/" (const "")
        compile copyFileCompiler
        
    match "scripts/*.js" $ do
        route   idRoute
        compile $ getResourceString 
            >>= withItemBody (unixFilter "./compressJS.sh" [])

    match "scripts/**/*.js" $ do
        route   idRoute
        compile $ getResourceString
            >>= withItemBody (unixFilter "./compressJS.sh" [])
{-
    match "css/style.css" $ do
        route   idRoute
        compile copyFileCompiler
-}

    match "css/complete.min.css" $ do 
        route $ constRoute "css/style.css"
        compile copyFileCompiler

    match "index.html" $ do
        route idRoute
        compile $ genCompiler tags (field "posts" $ \_ -> (postList "blog/*") $ fmap (take 5) . recentFirst)
                
    match "archive.html" $ do
        route idRoute
        compile $ genCompiler tags $ field "posts" ( \_ -> postListByMonth tags "blog/*" (recentFirst))
          
    match "talks.html" $ do 
        route idRoute
        compile $ genCompiler tags (field "posts" $ \_ -> (postList "talks/*.md") $ fmap (take 5) . (recentFirst >=> filterTalks))
                                                                
    match "talk-archive.html" $ do
        route idRoute
        compile $ genCompiler tags $ field "posts" ( \_ -> postListByMonth tags "talks/*" (recentFirst >=> filterTalks)) 

    match "basic/*" $ do
        route baiscRoute
        compile $ applyKeywords
                    >>= loadAndApplyTemplate "templates/main.html" (taggedPostCtx tags)

    match "blog/*" $ do
        route blogRoute
        compile $ do
            csl <- load "springer-lncs.csl"
            bib <- load "ref.bib"
            p  <- readPandocBiblio pandocReaderOptions csl bib <$> applyKeywords
            p' <- p
            saveSnapshot "teaser" $ writePandocWith pandocWriterOptions p'
            >>= loadAndApplyTemplate "templates/post.html" (taggedPostCtx tags)
            >>= loadAndApplyTemplate "templates/main.html" (taggedPostCtx tags)

    match "talks/*" $ do
        route blogRoute
        compile $ do
            csl <- load "springer-lncs.csl"
            bib <- load "ref.bib"
            p  <- readPandocBiblio pandocReaderOptions csl bib <$> applyKeywords
            p' <- p
            saveSnapshot "teaser" $ writePandocWith pandocWriterOptions p'
            >>= loadAndApplyTemplate "templates/talk.html" (taggedPostCtx tags)
            >>= loadAndApplyTemplate "templates/main.html" (taggedPostCtx tags)

    match "pages/*" $ do
        route blogRoute
        compile $ do
            csl <- load "springer-lncs.csl"
            bib <- load "ref.bib"
            p  <- readPandocBiblio pandocReaderOptions csl bib <$> applyKeywords
            p' <- p
            saveSnapshot "teaser" $ writePandocWith pandocWriterOptions p'
            >>= loadAndApplyTemplate "templates/post.html" (taggedPostCtx tags)
            >>= loadAndApplyTemplate "templates/main.html" (taggedPostCtx tags)

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend` bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots ("blog/*" .&&. hasNoVersion) "teaser"
            renderAtom myFeedConfiguration feedCtx posts

    create ["sitemap.xml"] $ do
        route idRoute
        compile $ do 
            posts <- recentFirst =<< loadAll "blog/*"
            talks <- recentFirst =<< loadAll "talks/*"
            pages <- loadAll "pages/*"
            basic <- loadAll "basic/*"
            let allPosts = (return (posts ++ talks ++ pages ++ basic))
            let sitemapCtx = mconcat [
                                listField "entries" minimalPageCtx allPosts
                                , constField "host" host
                                , defaultContext]
            makeItem "" 
                >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

    match ("templates/*" .||. "partials/*") $ compile templateCompiler

    match "ref.bib" $ compile biblioCompiler
    match "springer-lncs.csl" $ compile cslCompiler

--------------------------------------------------------------------------------

genCompiler :: Tags -> Context String -> Compiler (Item String)
genCompiler tags posts = 
              applyKeywords
                >>= applyAsTemplate posts
                >>= loadAndApplyTemplate "templates/main.html" (taggedPostCtx tags)

--------------------------------------------------------------------------------

baiscRoute :: Routes
baiscRoute = gsubRoute "basic/" (const "") `composeRoutes` setExtension "html"
         
blogRoute :: Routes
blogRoute = gsubRoute "pages/" (const "") `composeRoutes`
              gsubRoute "[0-9]{4}-[0-9]{2}-[0-9]{2}-" (map replaceChars) `composeRoutes` 
                setExtension "html" 
              where
                replaceChars c | c == '-' || c == '_' = '/'
                               | otherwise = c


--------------------------------------------------------------------------------
filterByType :: (MonadMetadata m, Functor m) => String -> [Item String] -> m[Item String]
filterByType tpe = filterM hasType
              where
                hasType item = do
                    metadata <- getMetadata $ itemIdentifier item
                    let typ = Data.Map.lookup "type" metadata
                    return (typ == Just tpe)

filterTalks :: (MonadMetadata m, Functor m) => [Item String] -> m[Item String]
filterTalks = filterByType "talk"

--------------------------------------------------------------------------------

feedContext :: Context String
feedContext = mconcat
    [ bodyField "description"
    , defaultContext
    ]

--------------------------------------------------------------------------------

tagCloudCtx :: Tags -> Context String
tagCloudCtx tags = field "tagcloud" $ const rendered 
  where rendered = renderLogTagCloud 0.8 1.8 "em" tags

taggedPostCtx :: Tags -> Context String
taggedPostCtx tags = mconcat [tagsField "tags" tags, tagCloudCtx tags, postCtx]

minimalPageCtx :: Context String
minimalPageCtx = mconcat [constField "host" host, modificationTimeField "lastmod" "%Y-%m-%d", defaultContext]

postCtx :: Context String
postCtx = mconcat [dateField "date" "%Y-%m-%d", modificationTimeField "lastmod" "%Y-%m-%d", abstractField "abstract", contentField "content", defaultContext]

abstractField :: String -> Context String
abstractField key = field key $ \item -> do 
  let body = (itemBody item) in 
    case needlePrefix "<!--more-->" body of 
      Nothing -> fail $ "No abstract defined for " ++ show (itemIdentifier item)
      Just t ->  return t

contentField :: String -> Context String 
contentField key = field key $ \item ->
  let body = (itemBody item) in 
    case needlePrefix "<!--more-->" body of 
      Nothing -> fail $ "No abstract defined for " ++ show (itemIdentifier item)
      Just t -> return $ drop (length t) body

--------------------------------------------------------------------------------

postLst :: Pattern -> Identifier -> Context String -> ([Item String] -> Compiler [Item String]) -> Compiler String
postLst pattern template context sortFilter = do
    posts   <- return =<< sortFilter =<< loadAll (pattern .&&. hasNoVersion)
    itemTpl <- loadBody template
    applyTemplateList itemTpl (teaserField "teaser" "teaser" `mappend` context) posts

postList :: Pattern -> ([Item String] -> Compiler [Item String]) -> Compiler String
postList searchPattern = postLst searchPattern "templates/post-item.html" postCtx

postListByMonth :: Tags
                 -> Pattern
                 -> ([Item String] -> Compiler [Item String])
                 -> Compiler String
postListByMonth tags pattern filterFun = do
    posts   <- bucketYear =<< filterFun =<< loadAll (pattern .&&. hasNoVersion)
    itemTpl <- loadBody "templates/month-item.html"
    monthTpl <- loadBody "templates/month.html"
    yearTpl <- loadBody "templates/year.html"
    let makeMonthSection :: ((String, String), [Item String]) -> Compiler (Item String); 
        makeMonthSection ((yr, mth), ps) =
            applyTemplateList itemTpl (taggedPostCtx tags `mappend` dateField "day" "%d") ps 
            >>= makeItem
            >>= applyTemplate monthTpl (monthContext yr mth)
    let makeYearSection :: ((String), [Item String]) -> Compiler (Item String) ; 
        makeYearSection ((yr), ps)  = 
             (concatMap itemBody <$> (mapM makeMonthSection =<< (bucketMonth ps)))
             >>= makeItem
             >>= applyTemplate yearTpl (yearContext yr)
    concatMap itemBody <$> mapM  makeYearSection posts
  where
    bucketYear posts =
        reverse . map (second (map snd)) . buckets (fst . fst) <$>
        mapM tagWithMonth posts
    bucketMonth posts =
        reverse . map (second (map snd)) . buckets fst <$>
        mapM tagWithMonth posts
    tagWithMonth p = do
        utcTime <- getItemUTC timeLocale $ itemIdentifier p
        return ((formatTime timeLocale "%Y" utcTime, formatTime timeLocale "%m" utcTime), p)
    timeLocale = defaultTimeLocale
 
monthContext :: String -> String -> Context String
monthContext year month = mconcat
    [ constField "year"  year
    , constField "month" $ convertMonth month
    , bodyField  "postsByMonth"
    ]

yearContext :: String -> Context String
yearContext year  = mconcat
    [ constField "year"  year
    , bodyField  "postsByMonth"
    ]
    
convertMonth :: String -> String
convertMonth "01" = "January"
convertMonth "02" = "February"
convertMonth "03" = "March"
convertMonth "04" = "April"
convertMonth "05" = "May"
convertMonth "06" = "June"
convertMonth "07" = "July"
convertMonth "08" = "August"
convertMonth "09" = "September"
convertMonth "10" = "October"
convertMonth "11" = "November"
convertMonth "12" = "December"

buckets :: Ord b => (a -> b) -> [a] -> [ (b,[a]) ]
buckets f = map (first head . unzip)
          . groupBy ((==) `on` fst)
          . sortBy (compare `on` fst)
          . map (\x -> (f x, x))

--------------------------------------------------------------------------------
          
