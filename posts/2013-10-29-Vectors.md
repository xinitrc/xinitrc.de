---
title: Vectors
tags: svg, tikz, hakyll
---

I promised some time ago to do a blog post on the tikz-2-svg converter I wrote for this blog. And after having snuck around keeping my promise for some time two things bring me to do it now. 

1. I haven't used it so far, but plan to do so in the very next blog post. 
2. I'm now part of an iron blogging group and so I have to do a blog post this week.

So let's get started on how this thing works.

<!--more-->


## tl;dr
Most people who can piece this together from this tl;dr wouldn't need the rest of the post, but anyways here we go:

1. Extract the tikz picture from the markdown source.
2. Write it into a separate valid TeX file.
3. Compile the TeX file with htlatex.
4. Throw away everything else but the resulting svg file
5. Insert the svg in place of the tikz picture.
6. Profit!

## Show us the money

So let's go through the code following the steps above. First extraction of the tikz picture. 
As I already explained [here](/2013/06/22/3.26-Lightyears-away.html) I added a keyword compiler stage to add youtube, vimeo and slideshare embedding (and actually already there promised to write this blog post). Part of that is also the tikz keyword, which works almost exactly like the ones for the other services. So let's have a look at the relevant part from  [KeywordReader.hs](https://github.com/xinitrc/xinitrc.de/blob/master/Plugins/KeywordReader.hs).

~~~~ {.haskell}
tikz :: Parser KeywordElement
tikz = try $ do 
         void $ string "§§tikz("
         options <- optionMaybe $ many1 $ noneOf ")"
         void $string ")§§"
         tikzImage <- many1 $ noneOf "§§"
         void $ string "§§endtikz§§"
         return $ Tikz options tikzImage
          
~~~~

As you can see, at least if you can read Haskell and Parsec, I'm parsing for a "§§tikz(" then maybe some options a ")§§" than anything that is not a "§§" and end with a "§§endtikz§§". this is not exactly a tikz picture definition as you would write it in a TeX file, but it's close enough for me. 

The parser result is put through processTikZs of [Tikz.hs](https://github.com/xinitrc/xinitrc.de/blob/master/Plugins/Tikz.hs):

~~~~ {.haskell}
processTikZs :: KeywordElement -> String
processTikZs t@(Tikz _ _) = renderObjDesc ts
    where ts = unsafePerformIO $ renderSVG t
processTikZs _ = error "Unexpeced tikzpicture"

~~~~

Where renderObjDesc simply puts a little wrapper around the resulting svg image we can later use for styling. 

~~~~ {.haskell}

renderObjDesc :: TikZInfo -> String
renderObjDesc (TikZInfo md5) = "<div class=\"tikz\"><img type=\"image/svg+xml\" src=\"/assets/tikzs/" ++
                               addExtension md5 "svg" ++ 
                               "\"></object></div>"
~~~~

What you can see hinted at here is, that since I can't give the files meaningful names I at least try to give them useful ones. The filenames of the temporary file and the resulting svg are derived from the md5 sum of the content. This should result in no two files colliding and accidentally overwriting another and me being able to test if I have to recompile the image if I change the blog post. 

## The heart of it all

~~~~ {.haskell}							   
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
~~~~

This is as simple as it is ugly and basically a reason for a refactoring in the near future, but I'll get to that. 

As you can see I start by creating the target directory, if it is missing. Save the old path to change back to it later and than switch to the target directory for my svg. If the file I have to write is already there, which I know from the md5 sum, I'm done, can return to the original directory and go on with the rest of the compilation phase. Should I on the other hand really have to do work, a temporary directory is created and the TeX file is written, as you can see I use <span class="tt">hputStrln</span> for the hole shenanigans, which is one of the reasons I say the code is in desperate need for a refactoring. Then I do a system call for htlatex on the file and if an svg file results I move it to the target directory and rename it to the md5 sum of it's former tikz contents. After that only some clean up is to be done in form of removing the temporary directory. 

The remaining thing to do is to give the svg it's proper size, which I could extract from the compilation. But since I want everything to be responsive and svgs scale pretty good, I do it with a little css magic in [style.scss](https://github.com/xinitrc/xinitrc.de/blob/master/css/style.scss)

~~~~ {.css}
.tikz img {
  display: block;
  margin-left: auto;
  margin-right: auto;

  max-width: 100%;
  height: auto;
}
~~~~

## The good, the bad and the ugly

As I already said the code works which is good, it's bad because I hardcoded several pieces in it, like the target path and it is outright ugly because it's more procedural than functional. So I'll probably will rewrite it so that I use the templating facility of hakyll like I did for the youtube embedding. Instead of <span class="tt">writeTikzTmp</span> I would than have a template TeX file and a context generated from the options and the contents between "§§tikz()§§" and "§§endtikz§§" and than only call htlatex on the generated file. But as always with refactorings where to get the time to do them.

## Pretty pictures

Before we end I obviously have to give you some pictures. 

So here is some finite automaton:

§tikz(->,>=stealth',shorten >=1pt,auto,node distance=2.8cm, semithick)§
  \tikzstyle{every state}=[fill=red,draw=none,text=white]

  \node[initial,state] (A)                    {$q_a$};
  \node[state]         (B) [above right of=A] {$q_b$};
  \node[state]         (D) [below right of=A] {$q_d$};
  \node[state]         (C) [below right of=B] {$q_c$};
  \node[state]         (E) [below of=D]       {$q_e$};

  \path (A) edge              node {0,1,L} (B)
            edge              node {1,1,R} (C)
        (B) edge [loop above] node {1,1,L} (B)
            edge              node {0,1,L} (C)
        (C) edge              node {0,1,L} (D)
            edge [bend left]  node {1,0,R} (E)
        (D) edge [loop below] node {1,1,R} (D)
            edge              node {0,1,R} (A)
        (E) edge [bend left]  node {1,0,R} (A);
§endtikz§

And the glider: 

§tikz(thick)§
\draw (0,0) grid (3,3);
\foreach \c in {(0,0), (1,0), (2,0), (2,1), (1,2)}
    \fill \c + (0.5,0.5) circle (0.42);
§endtikz§


So now take this code and run with it. If you need another look just go [this way](https://github.com/xinitrc/xinitrc.de) to the git repository for this blog.  
