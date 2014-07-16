---
title: Long and winding road to a responsive video
tags: jekyll, plugin, youtube, vimeo, code, responsive web design
type: article
bgimage: /assets/images/winding_road.jpg
position: tl
credit: jbdodane
crediturl: https://www.flickr.com/photos/jbdodane/
---

As already suspected in my [first blog post](/private/2012/12/15/Setting-up-Shop.html), the updates to this site
may never end. Just as I was done with adding plugins and
just some hours after I was done writing my
[first plugin](/private/2012/12/17/Let-me-do-the-math.html)
and writing up on it. I thought: "And what if I would like to add a video?" Read on for the ***amazing*** journey that began.

<!--more-->

Ok, I might have exaggerated just a tiny bit. It wasn't all that
exciting and in the end is wasn't that hard either, but it might still
be helpful for others so I'll post it here. 

Adding a youtube video is easy, you just copy the embed code from the
video and you are done with it, at least so I thought. But after doing
just that, Jekyll threw up a lot of
[Markdown](http://en.wikipedia.org/wiki/Markdown) errors and I only
got a parsing error block in the resulting html file. 

So I thought ok let's see if there is a Jekyll plugin that does at
least add the youtube video and found this
[Youtube Plugin](https://gist.github.com/1805814). This only made it
worse, but at least I could debug the error partially. Somehow Maruku
or Jekyll died on the "&". Removing that got me a video frame
and a broke the complete site.

Back to square one. I took the original embed code and dropped
parameter after parameter to find out that Jekyll's XML/XHTML-Parser
won't work with lone parameters like "allowfullscreen" (with full
right, I might add). So I changed it to allowfullscreen="" which fixed
it. That way I got from an XML error to a broken site, like in
the plugin case. After doing about everything I could think off,
resulting in the same broken page every time, I typed the code myself
and got a working site, without any idea why. After going character
by character the only difference I could make out was a space
character within the iframe definition. Why Jekyll doesn't work with
empty html-tags, I don't know. But what I do know is a simple space
character in iframe fixed it.

An obvious addition was [Vimeo](http://vimeo.com) support. Which was
probably the easiest thing you could think of. Barely more than
exchanging youtube for vimeo.

## The code
After I had that, I rolled it into a plugin. (The additional div
will be explained further down.)

~~~ ruby
module Jekyll
  class Vimeo < Liquid::Tag
    def initialize(name, id, tokens)
      super
      @id = id
    end

    def render(context)
      "\n<div class=\"elastic-video\">
      <iframe src=\"http://player.vimeo.com/video/#{@id}\" frameborder=\"0\" webkitAllowFullScreen=\"\" mozallowfullscreen=\"\" allowFullScreen=\"\"></iframe>
      </div>\n"
    end
  end

  class YouTube < Liquid::Tag
 
    def initialize(tagName, id, tokens)
      super
 
      @id = id
    end
 
    def render(context)
      "\n<div class=\"elastic-video\">
      <iframe src=\"http://www.youtube.com/embed/#{@id}\" frameborder=\"0\" allowfullscreen=\"\">
      </iframe></div>\n"
    end
 
  end
end

Liquid::Template.register_tag "youtube", Jekyll::YouTube
Liquid::Template.register_tag "vimeo", Jekyll::Vimeo


~~~

## Responsiveness
Now I got working video, but that was only half of what I
wanted. Missing still: The video should be responsive. I already did
that for another site, so this bit came in easy. I surrounded the
video with a div to identify it and perform the following css voodoo on it. 

~~~ css
.elastic-video {
	position: relative;
	padding-bottom: 56.25%;
	padding-top: 30px;
	height: 0;
	overflow: hidden;
}

.elastic-video iframe, .elastic-video object, .elastic-video embed {
	position: absolute;
	top: 0;
	left: 0;
	width: 100%;
	height: 100%;
}
~~~

## Example

So here is an example: to get
[this video](http://www.youtube.com/watch?v=7mXeJlFdZ2c) embedded and
surrounded with divs to make it responsive the
following snippet suffices.

{% youtube 7mXeJlFdZ2c%}

§youtube(7mXeJlFdZ2c)§

Similarly for vimeo videos.

Now have fun resizing your browser and using the plugin. 
