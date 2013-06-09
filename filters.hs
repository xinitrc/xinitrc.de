module Filters(applyKeywords) where 

import            Control.Applicative          ((<$), (<$>))
import            Control.Monad                (void, liftM)

import            Hakyll.Core.Item
import            Hakyll.Core.Compiler

import            Text.Parsec
import            Text.Parsec.String

newtype Keywords = Keywords
    { unKeyword :: [KeywordElement]
    } deriving (Show, Eq)
    
data KeywordElement
    = Chunk String
    | Escaped
    | Youtube String
    | Vimeo String
    deriving (Show, Eq)


readKeywords :: String -> Keywords
readKeywords input = case parse keywords "" input of
    Left err -> error $ "Cannot parse keywords: " ++ show err
    Right t -> t

keywords :: Parser Keywords
keywords = Keywords <$> (many1 $ chunk <|> escaped <|> youtube <|> vimeo)

chunk :: Parser KeywordElement
chunk = Chunk <$> (many1 $ noneOf "§")

escaped :: Parser KeywordElement
escaped = Escaped <$ (try $ string "§§")

youtube :: Parser KeywordElement
youtube = try $ do
        void $ string "§youtube("
        videoID <- many1 $ noneOf ")"
        void $ string ")§"
        return $ Youtube videoID

vimeo :: Parser KeywordElement
vimeo = try $ do 
          void $ string "§vimeo("
          videoID <- many1 $ noneOf ")"
          void $ string ")§"
          return $ Vimeo videoID
          
applyKeywords :: Item String -> Compiler (Item String)
applyKeywords item = do
  body <- applyKeywords' $ readKeywords $ itemBody item
  return $ itemSetBody body item

applyKeywords' :: Keywords -> Compiler String
applyKeywords' = liftM concat . mapM applyKWs . unKeyword
  where
    applyKWs (Chunk c) = return c
    applyKWs (Escaped) = return "§"
    applyKWs m@(Youtube vid) = return $ videoBody m
    applyKWs m@(Vimeo vid) = return $ videoBody m
    

common :: String -> String -> String 
common service vid = "<div class=\"elastic-video\"><iframe src=\"" ++ service ++ vid ++ "\" frameborder=\"0\" allowfullscreen=\"\"></iframe></div>\n"
  
videoBody :: KeywordElement -> String 
videoBody (Youtube vid) = common "http://www.youtube.com/embed/" vid
videoBody (Vimeo vid)   = common "http://player.vimeo.com/video/" vid
videoBody _ = error $ "Unexpeced videoConfig"


