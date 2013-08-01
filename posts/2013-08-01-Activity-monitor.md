---
title: Activity Monitor
tags: hakyll, activity, css
---

This post actually serves two purposes:

1. It indicates that I'm still alive.
2. Since I was asked about the <span class="tt">active</span> class in the navigation of this site, I'd like to explain how that works.

<!--more-->

I actually use some of the newer features of Hakyll 4.3 in this Blog e.g. the <span class="tt">partials</span> and <span class="tt">if</span>. To make the navigation indicator work I only need the <span class="tt">if</span>, but while I'm at it I can also explain a little about the general setup of this blog.

## tl;dr

Actually <span class="tt">if</span> is more like an "<span class="tt">isDefined</span>" in Hakyll you can just find out if a variable is set. So my solution to add the active class to the menu is quite simple:

1. Add a variable for every menu entry (blog, talks, contact, facts).
2. Set the variable to some value in the <span class="tt">yaml</span> front matter of the matching pages.
3. In the navigation partial check on every entry if the matching variable is set.
4. If so add the <span class="tt">active</span> class to the matching entry.

That's it.

## A work through the code

Let's make that a little more detailed, probably nobody understood it from the tl;dr anyway. I take the [facts](/facts.html) page to illustrate it, but it works the same for all the other entries.


Our journey starts at line 3 of the [facts.html](https://github.com/xinitrc/xinitrc.de/blob/master/facts.html):

~~~ {.yaml}
---
title: xinitrc facts
facts: true
---
~~~

The variable <span class="tt">facts</span> is initialised and set to <span class="tt">true</span>, as I already said the value is completely irrelevant, it suffices to add the variable at all. (I was actually tempted to set it to <span class="tt">false</span> but that would be outright evil.)

Next in Line 144ff of [site.hs](https://github.com/xinitrc/xinitrc.de/blob/master/site.hs)

~~~ { .haskell }
match ("facts.html" .||. "contact.html" )$ do
        route idRoute
        compile $ applyKeywords
                  >>= loadAndApplyTemplate "templates/main.html" (taggedPostCtx tags)
~~~

the facts page is matched and the [templates/main.html](https://github.com/xinitrc/xinitrc.de/blob/master/templates/main.html) is applied.

~~~ {.html }
<di id="wrapper">
    $partial("partials/navbar.html")$

    <div class="contentwrapper">
        <div class="content">
            $body$
        </div>
    </div>

    $partial("partials/sidebar.html")$
</div>

~~~

Where in Line 6 the [partials/navbar.html](https://github.com/xinitrc/xinitrc.de/blob/master/partials/navbar.html) partial is requested for rendering. Which in line 16ff contains the following code.

~~~ { .html }
<li>
    <a  $if(facts)$ class="active" $endif$ href="/facts.html">xinitrc facts<i class="icon-chevron-right con-large"></i></a>
</li>
~~~


This looks up if <span class="tt">facts</span> is defined, which it is, and therefor renders the <span class="tt"> class="active"</span>.

That's it. I hope this helps and you find some use for it somewhere else.