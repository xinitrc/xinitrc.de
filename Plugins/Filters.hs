module Plugins.Filters(applyKeywords) where 

import            Control.Monad                (void, liftM)

import            Hakyll.Core.Item
import            Hakyll.Core.Compiler

import            Plugins.Tikz
import            Plugins.KeywordReader

import Hakyll

applyKeywords :: Compiler (Item String)
applyKeywords = do
  b <- getResourceBody 
  aplKeywords b

aplKeywords :: Item String -> Compiler (Item String)
aplKeywords item = do
  body <- applyKeywords' $ readKeywords $ itemBody item
  return $ itemSetBody body item

applyKeywords' :: Keywords -> Compiler String
applyKeywords' = liftM concat . mapM applyKWs . unKeyword
  where
    applyKWs (Chunk c) = return c
    applyKWs (Escaped) = return "§"
    applyKWs m@(Youtube vid) = return $ videoBody m
    applyKWs m@(Vimeo vid) = return $ videoBody m
    applyKWs t@(Tikz _ _) = return $ processTikZs t
    

common :: String -> String -> String 
common service vid = "<div class=\"elastic-video\"><iframe src=\"" ++ service ++ vid ++ "\" frameborder=\"0\" allowfullscreen=\"\"></iframe></div>\n"
  
videoBody :: KeywordElement -> String 
videoBody (Youtube vid) = common "//www.youtube.com/embed/" vid
videoBody (Vimeo vid)   = common "//player.vimeo.com/video/" vid
videoBody _ = error $ "Unexpeced videoConfig"


