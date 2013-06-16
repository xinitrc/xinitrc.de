{-# LANGUAGE OverloadedStrings #-}

module Plugins.Filters(applyKeywords) where 
import            Control.Monad                (void, liftM)

import            Hakyll.Core.Item
import            Hakyll.Core.Compiler

import            Data.List                     (intersperse)

import            Plugins.Tikz
import            Plugins.KeywordReader


import            Hakyll

applyKeywords :: Compiler (Item String)
applyKeywords = do
  b <- getResourceBody 
  aplKeywords b

aplKeywords :: Item String -> Compiler (Item String)
aplKeywords item = do
  body <- applyKeywords' $ readKeywords $ itemBody item
  return $ itemSetBody body item

applyKeywords' :: Keywords -> Compiler String
applyKeywords' kws = (sequence $ map applyKWs $ unKeyword kws) >>= \x -> return $ concat $ map itemBody x
  where
    applyKWs (Chunk c) = makeItem c
    applyKWs (Escaped) = makeItem "§"
    applyKWs m@(Youtube vid) = youtube vid
    applyKWs m@(Vimeo vid) = vimeo vid
    applyKWs t@(Tikz _ _) = makeItem $ processTikZs t
    applyKWs (SlideShare sid) = slideShare sid
    
    
externalResource :: Identifier -> String -> String -> Compiler (Item String)
externalResource templateId fieldName id = do 
                  makeItem "" 
                  >>= loadAndApplyTemplate templateId (constField fieldName id)


youtube :: String -> Compiler (Item String)
youtube = externalResource "templates/youtube.html" "vid"


vimeo :: String -> Compiler (Item String)
vimeo = externalResource "templates/vimeo.html" "vid"

slideShare :: String -> Compiler (Item String)
slideShare = externalResource "templates/slideshare.html" "slidesID"