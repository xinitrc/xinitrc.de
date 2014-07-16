module Topics.Topics where
import XMonad
import qualified Data.Map as M
import qualified XMonad.StackSet as W

import XMonad.Actions.TopicSpace
import XMonad.Actions.GridSelect
import Control.Arrow

data TopicItem = TI 	{ topicName :: Topic
			, topicDir  :: String
			, topicAct  :: X ()
			}

myTopics :: [TopicItem]
myTopics = 	[ 
		   TI "main" "" (return ())
                ,  TI "mail" "" (spawnInTopicDir "urxvtc -e mutt")
                ,  TI "irssi" "" (spawnInTopicDir "urxvtc -e irssi" >> spawnInTopicDir "urxvtc -e irssi")
		,  TI "web" "" (spawn "chromium-browser")
                ,  TI "newsbeuter" "Development/private/newsbeuter" (spawnShell >> spawnInTopicDir "urxvtc -e emacs -nw")
                ,  TI "calendar" "" (spawn "chromium-browser --app='http://calendar.google.com'")
                ,  TI "xinitrc.de" "Development/private/xinitrc.de" (spawnInTopicDir "urxvtc -e emacs -nw" >> spawnShell  >> spawnShell)
                ,  TI "uberspace" "" (spawnInTopicDir "urxvtc -e ssh uberspace")                
--                ,  TI "org" "notes" (spawnInTopicDir "urxvt -e emacs -nw $(date +'%Y-W%U').org")
                ,  TI "xmonad" ".xmonad" (spawnInTopicDir "urxvtc -e emacs -nw *.hs lib/*/*.hs")
                ,  TI "feeds"  "" (spawn "chromium-browser --app='https://feedbin.me'")
                ,  TI "stats"  "" (spawnInTopicDir "urxvtc -e htop")
		]

myTopicNames :: [Topic]
myTopicNames = map topicName myTopics

myTopicConfig :: TopicConfig
myTopicConfig = TopicConfig
	{ topicDirs = M.fromList $ map (topicName &&& topicDir) myTopics
	, defaultTopicAction = const $ return ()
	, defaultTopic = "main"
	, maxTopicHistory = 10
	, topicActions = M.fromList $ map (topicName &&& topicAct) myTopics 
	}


spawnInTopicDir act = currentTopicDir myTopicConfig >>= spawnIn act

spawnShell :: X ()
spawnShell =  asks (terminal . config) >>= spawnInTopicDir

spawnShellIn :: Dir -> X ()
spawnShellIn dir = asks (terminal . config) >>= flip spawnIn dir

spawnIn act dir = spawn $ "cd " ++ dir ++ "; " ++ act

crizer :: String -> Bool -> X(String, String)
crizer _ False = return ("#002b36", "#839496")
crizer _ True = return ("#839596", "#002b36")

{-
crizer :: String -> Bool -> X(String, String)
crizer _ False = return ("#fdf6e3", "#657b83")
crizer _ True = return ("#657b83", "#fdf6e3")
-}
gsConfig = defaultGSConfig {
           gs_colorizer = crizer
        ,  gs_font = "xft:Source Code Pro:pixelsize=14"
}

wsgrid = withWindowSet $ \w -> do
    let wss = W.workspaces w
        usednames = map W.tag $  wss
        newnames = filter (\used -> (show used `notElem` (map show myTopicNames))) usednames
    gridselect gsConfig (map (\x -> (x,x)) (myTopicNames ++ newnames))
 
-- gridselect a workspace and view it
promptedGoto = wsgrid >>= flip whenJust (switchTopic myTopicConfig)
-- gridselect a workspace to shift active window to
promptedShift = wsgrid >>= flip whenJust (windows . W.shift)