---
title: Offline First
tags: offline first, barcamp oldenburg, cookies, localStorage, websql, indexdb, appcache, serviceworker
---

The last two days I was at the [Barcamp Oldenburg](http://www.barcamp-oldenburg.de/). Since my current employer was the organiser of said Barcamp, I felt lightly pressured into giving a session. In the end it went to be two different session. The first was about [GTD](/tags/gtd.html), basically the one I gave several times already. The newer and therefore maybe a bit more interesting one was about offline first.
As always I prepared some slides, which again are German (sorry) and can be found [here](/assets/documents/Offline-First.pdf).

<!--more-->

Anybody interested in further information should follow these links:

First there is [offlinefirst.org](http://offlinefirst.org), the community discussing offline first pattern, techniques and UI/UX.

## Client-Side Data Storage

For information on how to store information in the client I mentioned the following things in the talk. I would recommend them as an initial read and for first experiments in the topic.

[Client-Side Data Storage](http://shop.oreilly.com/product/0636920043676.do):
: The book from O'Reilly publishing I recommended for information on Cookies, LocalStorage, WebSQL and IndexDB.

[CouchDB](http://couchdb.org):
: CouchDB, the key-value store I mentioned in the talk.

[PouchDB](http://pouchdb.com):
: PouchDB, the client side/javascript implementation of the CouchDB API, which is compatible with the CouchDB sync protocol.

## Serviceworker

The probably most fun thing in the realm of offline first at the moment is probably the ServiceWorker, which I obviously mentioned in the talk. If you want more information on this, at least one of the following link should be interesting to you. 

[The ServiceWorker is coming, look busy](https://www.youtube.com/watch?v=SmZ9XcTpMS4):
: The talk by Jake Archibald I showed a snippet of explaining the usage of ServiceWorker.

[The offline cookbook](https://jakearchibald.com/2014/offline-cookbook/):
: An extensive list of offline first pattern one can implement using ServiceWorker.

[ServiceWoker cookbook](https://serviceworke.rs/):
: A list of examples one can do with ServiceWorker among others offline first pattern.

## Example App

The example demo app I showed during the talk, allowing to put notes on Githubbers, is also on github now. The repository is in a post Barcamp lack of creativity named [github notes](https://github.com/xinitrc/github-notes-example). 

Have fun Hacking on it. 