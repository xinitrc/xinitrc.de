---
title: Let me do the math
tags: jekyll, plugin, mathml, code
---

As this is my site and a big part of my life is math, I'd obiously like to have 
the posibility to use mathmatical formulae here. 

So after looking around for a while I found out, that [Maruku](http://maruku.rubyforge.org/) 
the standard Markdown engine of [Jekyll](http://jekyllrb.com) is able to use e.g. 
[ritex](http://ritex.rubyforge.org/), or [blahtex](http://gva.noekeon.org/blahtexml/) 
to convert Latex formulae into [MathML](http://en.wikipedia.org/wiki/MathML) or png 
images. A little investgation into Jekyll later I knew that it only uses blahtex for 
conversion of formulea into png images (if that's wrong drop me a line in the 
comments please). Since I really like modern web standards this was unacceptable for 
me and I took a stab into writing my first Jekyll plugin.

<!--more-->

As most of the formulae I use will be rather short, at least at first. I thought
I'd start with something that can do short formulae easy and find a solution for
the inevitable giant formula later. I still liked to keep my blog compiling 
automatically after
[I push it to my git repository](/blog/2012/12/10/Setting%2Bup%2BShop.html) 
and Latex is not installed on my hosting provider [uberspace](http://uberspace.de). 
I could do that, but just for having some formulea on my blog seemed a bit excessive. 
So I started out going for a pure ruby setup with ritex, just to find out that it can't
even parse a simple array environment. This was equally unacceptable. 

Next iteration: 
[itex2mml](http://golem.ph.utexas.edu/~distler/blog/itex2MML.html)
which is a standalone program in a single binary and has ruby bindings. After downloading and 
copying into my path, the next challange arose. How the hack is the ruby binding package 
called? Actually searching for it was no fun at all and didn't find me a solution either, 
so I went back and fumbled around in the Maruku source code since it is one of the supported
options for a rendering engine. With the plesant suprise that I could keep the code almost unchanged. 
And a little furious with the naming convention of this package. itex2mml, Itex2MML and itextomml 
come on! Who the heck does something like that. 

## The code

After that was sorted out it was pretty easy to come up with the following code. Feel free 
to use it for your own sites.

~~~ ruby
require 'itextomml'

module Jekyll
  class TexTag < Liquid::Tag
  
    def initialize (tag_name, text, tokens)
      super
      @text = text
    end 

    def render(context) 
      p = Itex2MML::Parser.new

      '<div class="mathLatexToMML">' << p.block_filter("#{@text}") << '</div>'
    end
  end
  class TexBlock < Liquid::Block
  
    def initialize (tag_name, text, tokens)
      super
    end 

    def render(context) 
      p = Itex2MML::Parser.new

      '<div class="mathLatexToMML">' << p.block_filter(super) << '</div>'
    end
  end
end 

Liquid::Template.register_tag('latex', Jekyll::TexTag)
Liquid::Template.register_tag('latexblock', Jekyll::TexBlock)

~~~

## Issues
Soon after I had my desired MathML I found out that Chrome doesn't support it (insert 
head-smashing-on-desk-noise here), oppose to all other browsers I interesetd in 
supporting. So I had a look around for some transparent fallback solution and found 
[MathJAX](http://www.mathjax.org) which probably everybody and their parents already 
knows, but anyhow, I'll explain what I did for those who don't.

It is actually extremely easy to configure MathJax as a MathML fallback. First you can
use the MathJax CDN and have the most current version of the library in use as soon as
it is released. The only thing you have to do is to tell MathJax to load the 
"MML_HTMLorMML.js" config and you are done.

So putting the following line in your html file will suffice.

~~~ html
<script type="text/javascript" src="https://c328740.ssl.cf1.rackcdn.com/mathjax/latest/MathJax.js?config=MML_HTMLorMML.js"></script>
~~~

## Results
Just as a small example, here are two formulae, one with the latex and one with the
latexblock liquid tag provided by the code above.

<pre>
{%latex e^{\pi\cdot i}+1=0 %}
</pre>
will result in: 
\(e^{\pi\cdot i}+1=0\)
and 

<pre>
{%latexblock%}
fib(n)=\pi_2 \left(\left(\begin{array}{cc}1 &amp; 1 \\ 1 &amp; 0\end{array}\right)^{n-1}\cdot\left(\begin{array}{c}1\\1\end{array}\right)\right)
{%endlatexblock%}
</pre>

will give you: 
\[fib(n)=\pi_2 \left(\left(\begin{array}{cc}1 & 1 \\ 1 & 0\end{array}\right)^{n-1}\cdot\left(\begin{array}{c}1\\1\end{array}\right)\right)\]

## Conclusion

I hope this is useful to someone else but me. If you have any ideas on improvements don't hesitat to drop me a line in the comments.
