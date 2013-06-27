module Plugins.Tikz(processTikZs) where

import Control.Monad (unless, when)

import System.Directory (doesFileExist, renameFile,
                         createDirectoryIfMissing, removeDirectoryRecursive,
                         getCurrentDirectory, setCurrentDirectory)

import System.IO (openFile, hPutStrLn, hPutStr, hClose, IOMode(..))
import System.Cmd (system)
import System.FilePath (addExtension)
import System.IO.Unsafe (unsafePerformIO)

import Data.Digest.Pure.MD5 (md5)
import Data.ByteString.Lazy.Char8 (pack)

import Plugins.KeywordReader (KeywordElement(..))

data TikZInfo = TikZInfo String deriving Show

processTikZs :: KeywordElement -> String
processTikZs t@(Tikz _ _) = renderObjDesc ts
    where ts = unsafePerformIO $ renderSVG t
processTikZs _ = error "Unexpeced tikzpicture"

renderObjDesc :: TikZInfo -> String
renderObjDesc (TikZInfo md5) = "<div class=\"tikz\"><img type=\"image/svg+xml\" src=\"/assets/tikzs/" ++
                               addExtension md5 "svg" ++ 
                               "\"></object></div>"
    
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
  setCurrentDirectory pwd
  return (TikZInfo md5)
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
makeDigest = show . md5 . pack
