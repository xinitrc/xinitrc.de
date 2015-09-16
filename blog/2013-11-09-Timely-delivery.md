---
title: Timely delivery
tags: angular, angularjs, subtle changes, ajax, $http, sanitization
bgimage: /assets/images/watchiii.jpg
position: tl
credit: primenerd
crediturl: https://www.flickr.com/photos/hiroic/
---

Yesterday AngularJS 1.2 was finally released and appropriately named "timely-delivery" (\*couch\*). After spending yesterdays evening porting one application I'm currently working on. I'd like to give some hints at subtle api changes that took me quite some time to find. Hopefully this helps others/you not to step into the same traps or solve similar problems faster.

<!--more-->

## $HTTP Service

The application I'm writing currently is based on the CANE Stack ([Couchdb](http://couchdb.apache.org), [AngularJS](http://angularjs.org), [Node.js](http://nodejs.org), [ExpressJS](http://expressjs.com)) one of the things I'm doing is to have all urls double as their own AJAX Endpoint. This is quite easy and looks as follows:

~~~ {.javascript}
exports.singlePageXHRRouteController = function (db, page) {
    return function (req, res) {
        pge = page || req.params.id;

        if(req.xhr) {
            db.get(pge, function (err, doc) {
                if (!err) {
                    res.send(doc);
                } else {
                    res.send(err);
                }
            });
        } else {
            routes.index(req,res);
        }
    }
}
~~~

This depends on ExpressJS ability to detect that the current request is an <span class="tt">xhr</span> (xhr = XMLHttpRequest, think ajax) request. This depends on the http's header field <span class="tt">X-Requested-With</span> to be set to <span class="tt">XMLHttpRequest</span>. AngularJS up to 1.0.8, the now second to last stable version, always set this field, but this changed in 1.2. Which leads to ExpressJS not getting that the ajax request really is one and requesting the same file over and over again. For me this was almost undebuggable, because obviously your browser crashes after going through the same cycle over and over again. 


After finding the bug, at least fixing it was quite easy. You just have to add the following line at an appropriate place in your AngularJS application and you are golden.

~~~ {.javascript}
app.config(['$httpProvider', function($httpProvider) {
	$httpProvider.defaults.headers.common['X-Requested-With'] = 'XMLHttpRequest';
}]);
~~~

## ng-bind-html-unsafe / ng-bind-html /ng-sanitize / $sce
Yes, I know that is a mouthful of a heading, but it's very well deserved for this section. Former stable versions of AngularJS provided you with the <span class="tt">ng-bind-html-unsafe</span> directive, which allowed you to input raw html at some point in your template. Usually this is a bad idea, just think of rendering user comments this way. For my application though it was perfectly ok, I save content as markdown in a body field of a CouchDB document, which is sanitized on input and rendered with [showdown](https://github.com/coreyti/showdown) on output. This is done via a markdown filter, but now you can't use <span class="tt">ng-bind</span> anymore because it will sanitize the output and you have html as text in your template. 

With Angular 1.2 ng-bind-html-unsafe is gone and has been replace with ng-bind-html which doesn't allow you to bind "unsafe" html anymore. You can make something "safe" for Angular by using the <span class="tt">sce</span> mechanism. If something is run through <span class="tt">$sce.trustAsHtml</span> or it's companions for urls, javascript and so on, it is considered safe. 

For the usual case you don't have to touch <span class="tt">$sce</span> at all you can just go with <span class="tt">ng-sanitize</span>, which you just have to add as a dependency to your application (and load the javascript obviously). <span class="tt">ng-sanitize</span> does the rest. Problem being that there seems to be a bug in it, at least for me it removes relative path from <span class="tt">img</span>-tags which is not really what I would want.

As I told you before in my case none of this is a problem, because I can trust my markdown to be pre-sanitized, as much as I can trust the third party sanitizer, I only had a glance at. As a simple workaround for the time it takes to either explain why relative path are a problem or fixing the bug, I had to work around that. Since I didn't want to break open all my controllers, add a new dependency and rewrite every occurrence of <span class="tt">$scope.body = data.body</span>, which I would have to introduce in the first place because I currently can pipe everything from CouchDB directly to the template, I wrote a simple filter which I can add in the template files. It looks like this:

~~~{.javascript}
app.filter('trustAsHTML', ['$sce', function($sce) {
    return function(input) {
        return $sce.trustAsHtml(input);
    }
}]);
~~~

So now this

~~~{.html}
<div ng-bind-html-unsafe="value.body | markdown"></div>
~~~~

becomes 

~~~{.html}
<div ng-bind-html="value.body | markdown | trustAsHTML"></div>
~~~

and that's all.

## Last remarks

Even though there are these small road bumps. I really like the new version of Angular, the more modularised structure, better promises and the <span class="tt">ng-animate</span> directive in particular. Should I find more kinks and workarounds for them I'll keep you posted. As always, you can obviously take the little source code I put here for your own projects. But seriously think about using the <span class="tt">trustAsHtml</span> filter, used on unsanitized user input it is dangerous. Or to say it in the immortal words of Uncle Ben:

> With great power there must also come — great responsibility!
> <small>Uncle Ben &mdash; Amazing Fantasy #15, August 1962 (the first Spider-Man story)</small>
