module Plugins.KeywordReader(Keywords(..), KeywordElement(..), readKeywords) where 

import            Control.Applicative          ((<$), (<$>))
import            Control.Monad                (void)

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
    | SlideShare String
    | Tikz (Maybe String) String
    deriving (Show, Eq)
             

readKeywords :: String -> Keywords
readKeywords input = case parse keywords "" input of
                       Left err -> error $ "Cannot parse keywords: " ++ show err
                       Right t -> t

keywords :: Parser Keywords
keywords = Keywords <$> many1 (chunk <|> escaped <|> youtube <|> vimeo <|> slideshare <|> tikz)

chunk :: Parser KeywordElement
chunk = Chunk <$> many1 (noneOf "§")

escaped :: Parser KeywordElement
escaped = Escaped <$ try (string "§§")

simpleIdParserGenerator :: String -> (String -> KeywordElement) -> Parser KeywordElement
simpleIdParserGenerator identifier constructor = try $ do
                                                   void $ string ("§"++ identifier ++ "(")
                                                   embedId <- many1 $ noneOf ")"
                                                   void $ string ")§"
                                                   return $ constructor embedId

slideshare :: Parser KeywordElement
slideshare = simpleIdParserGenerator "slideshare" SlideShare

youtube :: Parser KeywordElement
youtube = simpleIdParserGenerator "youtube" Youtube

vimeo :: Parser KeywordElement
vimeo = simpleIdParserGenerator "vimeo" Vimeo

tikz :: Parser KeywordElement
tikz = try $ do 
         void $ string "§tikz("
         options <- optionMaybe $ many1 $ noneOf ")"
         void $string ")§"
         tikzImage <- many1 $ noneOf "§"
         void $ string "§endtikz§"
         return $ Tikz options tikzImage
          
