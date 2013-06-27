module Plugins.Tikz(processTikZs) where

import Control.Monad (forM, unless, when)
import Data.List (isPrefixOf)
import System.Directory (doesFileExist, renameFile,
                         createDirectoryIfMissing, removeDirectoryRecursive,
                         getCurrentDirectory, setCurrentDirectory)
import System.IO (openFile, hPutStrLn, hPutStr, hClose, IOMode(..))
import System.FilePath (addExtension)
import System.Cmd (system)
import System.IO.Unsafe (unsafePerformIO)
import Data.Digest.Pure.MD5
import qualified Text.HTML.TagSoup as TS
import qualified Data.ByteString.Lazy.Char8 as C8
import Plugins.KeywordReader

import Hakyll

data TikZInfo = TikZInfo { digest :: String,
                           w :: Int, h :: Int
                           } deriving Show


processTikZs :: KeywordElement -> String
processTikZs t@(Tikz _ _) = renderObjDesc ts
                              where ts = unsafePerformIO $ renderSVG t
processTikZs _ = error "Unexpeced tikzpicture"

renderObjDesc :: TikZInfo -> String
renderObjDesc (TikZInfo md5 w h )= "<div class=\"tikz\"><img type=\"image/svg+xml\" src=\"/assets/tikzs/" ++
           addExtension md5 "svg" ++
           "\" width=" ++ show w ++
           " height=" ++ show h ++
           "></object></div>"
  
  
renderSVG :: KeywordElement -> IO TikZInfo
renderSVG (Tikz options tikz) = do
  createDirectoryIfMissing True "_site/assets/tikzs"
  pwd <- getCurrentDirectory
  setCurrentDirectory "_site/assets/tikzs"
  exists <- doesFileExist svgf
  unless exists $ do
    createDirectoryIfMissing True "tmp"
    setCurrentDirectory "tmp"
    writeTikzTmp "tmp.tex" options tikz 
    system "htlatex tmp.tex 2>&1 > /dev/null"
    status <- doesFileExist "tmp-1.svg"
    setCurrentDirectory ".."
    when status $ renameFile "tmp/tmp-1.svg" svgf
    removeDirectoryRecursive "tmp"
  (w, h) <- getSVGDimensions svgf
  setCurrentDirectory pwd
  return (TikZInfo md5 w h)
    where svgf = addExtension md5 "svg"
          md5 = makeDigest tikz

optionPrint :: Maybe String -> String
optionPrint (Just s) = "[" ++ s ++ "]"
optionPrint Nothing = ""

writeTikzTmp :: String -> Maybe String -> String -> IO ()
writeTikzTmp f options tikz = do
  h <- openFile f WriteMode
  hPutStrLn h "\\nonstopmode"
  hPutStrLn h "\\documentclass{minimal}"
  hPutStrLn h "\\def\\pgfsysdriver{pgfsys-tex4ht.def}"
  hPutStrLn h "\\usepackage{tikz}"
  hPutStrLn h "\\usetikzlibrary{arrows,automata,backgrounds,calc,calendar,er,intersections,mindmap,matrix,folding,patterns,plothandlers,plotmarks,shapes,snakes,topaths,trees}"
  hPutStrLn h "\\begin{document}"
  hPutStr h "\\begin{tikzpicture}"
  hPutStrLn h $ optionPrint options
  hPutStrLn h tikz
  hPutStrLn h "\\end{tikzpicture}"
  hPutStrLn h "\\end{document}"
  hClose h


makeDigest :: String -> String
makeDigest = show . md5 . C8.pack

getSVGDimensions :: String -> IO (Int, Int)
getSVGDimensions svgf = do
  cont <- readFile svgf
  let svgtags = filter (TS.isTagOpenName "svg") $ TS.parseTags cont
  case svgtags of
    [] -> error "Failed to find \"svg\" tag in TikZ-rendered SVG file"
    _ -> return $ extractDimensions $ head svgtags
  where extractDimensions :: TS.Tag String -> (Int, Int)
        extractDimensions tag = (w, h)
          where ws = (fst . head . lex) $ TS.fromAttrib "width" tag
                hs = (fst . head . lex) $ TS.fromAttrib "height" tag
                w = floor (scale * read ws :: Float)
                h = floor (scale * read hs :: Float)
                scale = 1.35

strip :: String -> String
strip = takeWhile (not . isWs) . dropWhile isWs
  where isWs c = c `elem` " \t\r\n"
