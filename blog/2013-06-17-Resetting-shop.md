---
title: Resetting shop
tags: jekyll, hakyll, bootstrapping
---

After having set up this site as a [Jekyll](http://jekyllrb.com/)-Blog last year, I was quite happy with what I had worked out for blogging. If you want to know, you can look it up in [this article](/blog/2012/12/15/Setting-up-Shop.html). But after not having an eye on the site for some month all hell broke loose and I had to start over.

<!--more-->

## The crisis

Two weeks ago I finally had time again, after some stressful month privately and at work and wanted to pick up blogging again. Much to my dismay, since last using Jekyll, the [Ruby](http://www.ruby-lang.org/) and Jekyll versions changed significantly and I had already upgraded Ruby and left the whole system in an non-compiling state. This was especially unconvenient since I know almost no Ruby and am even less able to debug strange problems with the packaging system.

## Twitter to the rescue
One tweet:

<div style="margin-left: auto; margin-right:auto">
<blockquote class="twitter-tweet" data-partner="tweetdeck" align="center"><p>I royally fucked my <a href="https://twitter.com/search?q=%23ruby&amp;src=hash">#ruby</a> setup. Which wouldn’t be have as bad if I wasn’t using Jekyll for blogging <a href="https://twitter.com/search?q=%23headdesk&amp;src=hash">#headdesk</a></p>&mdash; xinitrc (\@xinitrc) <a href="https://twitter.com/xinitrc/statuses/341619389919010817">June 3, 2013</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>
</div>

and especially one answer

<blockquote class="twitter-tweet" data-conversation="none" align="center" data-partner="tweetdeck"><p><a href="https://twitter.com/xinitrc">\@xinitrc</a> Use Hakyll instead.</p>&mdash; Dave Fayram (\@KirinDave) <a href="https://twitter.com/KirinDave/statuses/341620237030019072">June 3, 2013</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

later, I knew about [Hakyll](http://jaspervdj.be/hakyll/). And since I know [Haskell](http://www.haskell.org) far better than Ruby it was a done deal to switch over. 

## The challenge 
Obviously I wanted to have (at least) the same functionality with Hakyll I had been able to piece together with Jekyll. This included the:

* [MathML setup](/blog/2012/12/17/Let-me-do-the-math.html), 
* the [responsive videos](http://localhost:8000/2012/12/19/Long-and-winding-road-to-a-responsive-video.html), 
* the [talks page](/talks.html), which is a filtered index page, and
* the bucketed [archive pages](/archive.html), I didn't brag about before,

and if possible I'd like to have it easier, more robust, even more bells and whistles and fairy dust. Which should all be very well within the limits of what I could achieve with Haskell. 

**Yes, including the fairy dust.**

## The (new) setup

So here is the basic setup. Since my awesome hosting provider [uberspace](http://uberspace.de) has a ruby environment set up automatically I could write blog posts locally, <span class="tt">git commit</span> and <span class="tt">git push</span> them and have a <span class="tt">post-receive</span> hook generate the page server site. The same can, unfortunally, not be said for a Haskell environment. And since I deemed it a little overkill to put one up, just for the purpose of blogging, I decided to do more work locally. So now I'm write and compile the site locally and deploy it via Hakylls <span class="tt">deploy</span> command, which wraps a call to <span class="tt">rsync</span> over <span class="tt">ssh</span>.

As you can see from the sites linked further up still working, I maintained all the functionality mentioned above. 

For the MathML part this was rather easy since pandoc, the underlying compiler for Hakyll, already provides you with compilation to MathML out of the box. You merely have to switch it on, or better not switch it off. 

For the responsive video functionallity I went a little fancy and wrote small [Parsec](http://www.haskell.org/haskellwiki/Parsec) parser and while at it added responsive [Slideshare](http://www.slideshare.net/) embedding and a [Tikz](http://www.texample.net/tikz/) to [SVG](https://en.wikipedia.org/wiki/Scalable_Vector_Graphics) converter. Which allows me to write standard tikz code and have it automatically compiled to an SVG and embedded in place (as long as your browser supports it and it is not to complex). 

For the talk page I wrote some general filter code, which I am currently using only for filtering talks, but you never know what good this might be for later. 

And finally for the bucketed archive pages I also wrote a little code, which is not as general as I would like it to be, but that might change. 

If you are interested you can get the source code at [github](https://github.com/xinitrc/xinitrc.de). 
And I will devote some blog posts to the video and slideshare embedding, the tikz compiler, the filtering and the bucketing over the course of the next few days.
 

## Conclusion

After only 6 hours I had ported almost all of the functionality I had on my Jekyll page to Hakyll. With a code quality about comparable to the one in the Ruby site. In other words some rounds of refactoring where in order. But after devoting about an hour each day to refactoring the last week, I'm now pretty happy with the result. 

The worst thing of all is that I hadn't found Hakyll earlier. That could have saved me some valuable time.