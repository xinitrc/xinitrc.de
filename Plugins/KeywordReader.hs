module Plugins.KeywordReader where 

import            Control.Applicative          ((<$), (<$>))
import            Control.Monad                (void, liftM)

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
    | Tikz (Maybe String) String
    deriving (Show, Eq)


readKeywords :: String -> Keywords
readKeywords input = case parse keywords "" input of
    Left err -> error $ "Cannot parse keywords: " ++ show err
    Right t -> t

keywords :: Parser Keywords
keywords = Keywords <$> (many1 $ chunk <|> escaped <|> youtube <|> vimeo <|> tikz)

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
          
tikz :: Parser KeywordElement
tikz = try $ do 
        void $ string "§tikz("
        options <- optionMaybe $ many1 $ noneOf ")"
        void $string ")§"
        tikz <- many1 $ noneOf "§"
        void $ string "§endtikz§"
        return $ Tikz options tikz
          
