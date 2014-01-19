---
title: Reduce to the max
tags: xmonad, configuration, minimalism
published: 2014-01-19 23:00
---

For the last 10 years now I am a Apple user. Nevertheless I really like Linux and that's not only on servers and in embedded devices but for the Desktop. I actually don't see Linux fitting on the Desktop of anybody who is not tech savvy even though Ubunutu is not that bad. But for so called power users Linux actually is a very valid Desktop environment. So as a second Workspace I have VMWare Linux instance running on my machine almost 24-7. And since I have been asked now at least one to many times how my "strange" Desktop Environment is configured. I'd like to share that with you.

<!--more-->

## Tiling Window Manager

I've long been a fan of tiling window managers, especially since they work very well for most of the task I do most of the day. For writing papers or exams for students I need three windows one editor, one pdf viewer, and a terminal for compiling and vcs. For programming editor, documentation and a terminal for compiling and vcs. You get the point I think. What really helps is having them all side by side and not have to move one window out of the way of the others all the time to see, and this is were tiling window managers comes in. All windows are layed out side by side, actually in my setup that is not completely true, but we come to that. 

## XMonad

As you can guess by the side generator I use for this side (Hakyll) I really like Haskell. So deciding to use XMonad some years ago was more or less a given. Xmonad feels more like a window manager construction kit and is highly configurable, which is probably the reason why there are several example configurations on the XMonad [Wiki](http://www.haskell.org/haskellwiki/Xmonad/Config_archive). And since I could convince a student to do his bachelor thesis on usability enhancements for tiling window managers in XMonad and let him implement some of the features I was missing, and these enhancements even found their way into [XMonad core](http://xmonad.wordpress.com/2009/12/06/bluetile.-branch-merged-into-xmonad/) XMonad is now more or less the Window Manager of my dreams. Thanks again Jan for Bluetile! 

### Layouts
Obviously my configurations are attached nevertheless I walk you through some of what I did. For layouts I like following four very much. A main window the filling the left 70% of the screen and all the other windows lined up right to it top to bottom with 30% screen width and height depending on their number. Almost the same goes for the second layout which is the top 70% one window the rest sharing the bottom 30%. For these two I have obviously set up to have more than one window in the 70% area and shortcuts to change the ration. The third Layout I use is just one full screen app where I can only change which one that should be if I have more than one on the workspace. And the last one is Accordion where the focused window takes up most of the screen estate. All other applications share about 10% of the space only showing maybe one line of text. If I change the focused window that one goes bigger and all other retains their positions but shrink. This is usefull especially for writing papers where I usually only need to see the result while I'm not editing, so I can switch focus the editor minimizes and the pdf viewer maximises.


### Topic Spaces 
The absolute killer feature of XMonad for me are the Topic Spaces. You can think of them as Multiple Workspaces on steroids. A Topic Space is a workspace plus a default directory plus a configuration of applications to start on that workspace. Maybe this get's easier to understand with an example. As I already wrote in the introduction my day job often consists of writing papers, the usual workflow for that is: 

1. I open a terminal 
2. I set up a directory for the Tex Sources.
3. I initialize some vcs in that directory
4. I open a editor to work on the source files
5. I compile the source 
6. I open a pdf viewer to look at the result

this window configuration of a pdf viewer (that auto updates with every compilation) the terminal and the editor stay open in the directory the whole time. So after the initial creation of the directory I can configure a new topic space with the directory I write the paper in and the application configuration "Start a Terminal (in the specified directory), Start an editor and start a pdf view. Now everytime I change to my paper writing workspace the three apps spawn in the directory and I can start working in an instance. Same goes for my blog post writing, programming, xmonad configuration, mail, irc, ... workspace. 


### Some gimmicks
In addition to these I added some features to speed up specific workflows. One is having a new named workspace, I hit Meta-+ get a prompt asking for a name and after hitting Return I got a new workspace. Hitting Meta-s gives me a prompt I can use for web searches or opening an url, I either type "http://..." for an url  (yes that's like in the stone ages, but that's the price I have to pay) or just a word to search in google or prefix the search with one of serveral keywords like "amazon:" "imdb:", ... and search there. Last but not least GridSelect on pressing Meta-t I get 2D Grid with all workspaces I have configured (or later created). The ones I created earlier, i.e., further to the top in the configuration are more to the center and reachable with fewer keystrokes of the arrow keys. Same goes for Meta-g for all open applications. And last but not least I have Meta-Space giving me a dmenu to launch any application by typing an infix (most of the time I go with a prefix). 

As a little visual gimmick I also added is FadeInactive, which makes windows not having the focus 25% transparent. For me this helps with distractions. I fine tuned the value so that if I am concentrated on the focused window I don't even notice movement in those windows, like new messages on irc, but if I look specifically at that window I can still make out what is written there so that I know if I have to react. 

## Here are the files

As alway I hope this proves usefull for somenone so I give you the files: The main file [xmonad.hs](/assets/documents/xmonad.hs) and the [Topics.hs](/assets/documents/Topics.hs) which generates the topic space configuration. You will obviously want to change the configuration to your needs, but I think it should be obvious how to do that. 

And before I get asked where to put these files, the xmonad.hs goes in your .xmonad directory the Topic.hs in .xmonad/lib/Topics/ and than everything should work out fine after a <span class="tt">xmonad --recompile</span>.

And just for the reference here are some screenshots of the Desktop

[![Desktop Screenshot](/assets/images/Desktop_thumb.jpg)](/assets/images/Desktop.jpg)

and the creation of this blog post.

[![Blogpost Screenshot](/assets/images/Blogpost_thumb.jpg)](/assets/images/Blogpost.jpg)

Happy tiling.
