import XMonad

import Data.Ratio
import Topics.Topics

import XMonad.StackSet(swapUp, swapDown)

import XMonad.Util.Run
import XMonad.Util.EZConfig

import XMonad.Actions.Search
import XMonad.Actions.GridSelect
import XMonad.Actions.TopicSpace
import XMonad.Actions.DynamicWorkspaces

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.FadeInactive

import XMonad.Layout.Spacing
import XMonad.Layout.Minimize
import XMonad.Layout.Accordion
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.BoringWindows(boringAuto, focusUp, focusDown)

import XMonad.Prompt

import System.Exit


base03  = "#002b36"
base02  = "#073642"
base01  = "#586e75"
base00  = "#657b83"
base0   = "#839496" 
base1   = "#93a1a1"
base2   = "#eee8d5"
base3   = "#fdf6e3"
yellow  = "#b58900"
orange  = "#cb4b16"
red     = "#dc322f"
magenta = "#d33682"
violet  = "#6c71c4"
blue    = "#268bd2"
cyan    = "#2aa198"
green   = "#859900"

fg = base0
bg = base03

tiled = Tall nmaster delta ratio
	where 
		nmaster = 1
		ratio = 70/100
		delta = 1/50

myLayout = smartBorders $ spacing 5 $ boringAuto $ minimize (tiled ||| Mirror tiled ||| Accordion ||| Full) 

myDzenPP h = dzenPP {
                       ppCurrent = dzenColor bg fg . wrap "^p(5)" "" . pad 
		    ,  ppVisible = dzenColor blue bg . wrap "^p(5)" "" . pad
                    ,  ppLayout  = const ""
                    ,  ppHidden  = const ""
                    ,  ppHiddenNoWindows = const ""
		    ,  ppUrgent  = dzenColor red bg . wrap "^p(5)" "" . pad
		    ,  ppTitle   = dzenColor yellow bg . dzenEscape
                    ,  ppSep = "  "
		    ,  ppOutput  = hPutStrLn h
                }

myFont = "xft:Source Code Pro:pixelsize=14"

myManageHook = composeAll 
	[
		className =? "MPlayer" --> doFloat
	]

data Prom = Prom String
instance XPrompt Prom where 
	showXPrompt (Prom x) = x

myXPConfig = greenXPConfig { font = myFont, fgColor = fg, bgColor = bg, fgHLight = bg, bgHLight = fg, position = Top } 
newWS = mkXPrompt (Prom "Name for Workspace: ") myXPConfig (mkComplFunFromList []) addWorkspace

dzenGeneralOptions = " -bg '" ++ bg ++ "' -fg '" ++ fg ++ "' -fn '" ++ myFont ++ "'"

main = do
	statuspipe <- spawnPipe $ "/usr/local/bin/dzen2 -ta l -w 1750 " ++ dzenGeneralOptions
        spawn $ "conky -b -c ~/.conky/conky_time | /usr/local/bin/dzen2 -ta r -x 1750 " ++ dzenGeneralOptions
        spawn "ssh-add"
        spawn "urxvtd -f" 
        xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig { 
		modMask = mod4Mask, 
		terminal = "urxvtc",
		normalBorderColor = cyan,
		focusedBorderColor = blue,
		borderWidth = 1,
		workspaces = myTopicNames, 
		logHook = (fadeInactiveLogHook 0.75) >> (dynamicLogWithPP $ myDzenPP statuspipe),
		layoutHook = avoidStruts (myLayout),
		manageHook = myManageHook,
		keys = \c@(XConfig {XMonad.modMask = modm}) -> (mkKeymap c $
				[ ("M-<Space>", spawn $ "exec $(dmenu_path | /usr/local/bin/dmenu -nb '" ++ bg ++ "' -nf '" ++ fg ++ "' -sb '" ++ fg  ++ "' -sf '" ++ bg ++ "' -fn " ++ "'Source Code Pro:pixelsize=14')"),
			          ("M-<Tab>", focusDown),
			          ("M-S-<Tab>", focusUp),
			          ("M-n", spawnShell),
			          ("M-r", refresh),
			          ("M-l", sendMessage NextLayout),
			          ("M-<Left>", sendMessage Shrink),
			          ("M-<Right>", sendMessage Expand),
                                  ("M-.", sendMessage (IncMasterN 1)),
                                  ("M-,", sendMessage (IncMasterN (-1))),
			          ("M-<Up>", windows swapUp),
			          ("M-<Down>", windows swapDown),
			          ("M-S-q", io (exitWith ExitSuccess)),
			          ("M-S-r", spawn "xmonad --recompile; killall dzen2 conky; xmonad --restart"),
			          ("M-S-c", kill),
			          ("M-g", goToSelected defaultGSConfig),
			          ("M-h", withFocused minimizeWindow),
			          ("M-S-h", sendMessage RestoreNextMinimizedWin),
			          ("M-t", promptedGoto),
			          ("M-S-t", promptedShift),
			          ("M-+", newWS),
			          ("M--", removeWorkspace),
                                  ("M-s", promptSearch myXPConfig $ intelligent multi)
 			        ])	
	}
