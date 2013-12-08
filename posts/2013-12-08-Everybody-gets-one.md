---
title: Everybody gets one
tags: couchdb, per-user database, example
---

In the recent past I started working on a document management system for our department at the university again. And since I like CouchDB very much and it is perfect for the job due to it's superb replication abilites I opted for using it. As part of that is a quite involved replication scheme, someone going to a conference and only having restricted or flunky internet access, like I had three month ago while on a conference in Shanghai, is a common case in academia. In search of some examples on replication setups I touched on per-user databases, which are commonly tossed around for restricting read access in CouchDB. What I couldn't find actually was an example on how to set those up. So this is an attempt to explain per-user databsaes in a modestly complex example.

<!--more-->

## Premise

Let's say part of your application is a (private) messaging system. Since from time to time someone is going on the road, and has only limited access to the internet, you want to allow them to replicate their messages to their local maschine to have offline access to them. 

## Problem 

Since CouchDB restricts read access on a per-database level, i.e., if you have read access to a database you can read the whole of it, anybody who has a look at the replication endpoints you set up now has access to all private messages. This is done simply by setting up their own replication, independent of your application. 


## The solution

The solutions are per-user databases. They work basically as follows: Every user, at least for the messages, get's their own database, which due to filtered replication only contains those messages where the specified user is sender or recepient. Since this filtered replication is done on the server that user can't change the replication scheme. If you now replicate the data for offline usage, you only replicate the user's database. Even if that user is savy enough to find out the replication endpoints he can't get to the private messages of the other users, since they aren't actually in the database.


## The setup

Now let us set up such a system. Let's assume that we have an admin set up on the central CouchDB instance called "admin" with password "password", that the database in question is called "messages" and that a message contain one field sender, and one recepient field, like this:

~~~ {.javascript}
{
	...
	'sender': 'sender',
	'recipient': ['recipient1', 'recipient2'],
	...
}
~~~

We can now write a simple filter function and put it in a design document: 

~~~ {.javascript}
{
	'_id': '_design/filters'
	'filters': {
		'userMessageFilter': 'function (doc, req) { return doc.sender === req.params.user || doc.recipient.indexOf(req.params.user) !== -1; }'
		}
}
~~~

this will filter out all documents where the user given as a parameters is neither sender nor receiver. 

We now restrict access to the messages database to only the admin user simply by setting the "_security" property. 

~~~ {.bash}
curl -X POST http://admin:password@localhost:5984/messages/_security -d '{"admins": { "names": ["admin"], "roles": [] }, "members":{ "names": ["admin"], "roles": [] } }' -H "Content-Type: application/json"
~~~

Next we can create a user 

~~~ {.bash}
curl -X POST http://admin:password@localhost:5984/_users -d '{"_id": "org.couchdb.user:user1", "type": "user", "name": "user1", "roles": [], "password": "123"}' -H "Content-Type: application/json"
~~~ 

and a matching database

~~~ {.bash}
curl -X PUT http://admin:password@localhost:5984/user1messages'
~~~

and set it to be only readable by that user

~~~ {.bash}
curl -X POST http://admin:password@localhost:5984/user1messages/_security -d '{"admins": { "names": [""], "roles": [] }, "members":{ "names": ["user1"], "roles": [] } }' -H "Content-Type: application/json"
~~~

Let's review: So far we have two databases "messages" and "user1messages". The "messages" database can only be accessed by admin, the user1message only by user1. The "messages" database contains all messages from and to all users in the system. To now let user1 read their messages we need to set up replication.


### Replication

What we want is that if someone writes a message to user1 it get's into the "messages" database is replicated to "user1messages" where user1 can then read it. As well as the other way around user1 writes a message to someone put's it into "user1messages" this get's replicated to "messages" and on the other side back down into the recipients respective databases.

First the direction "messages" to "user1messages" we have to put the following document into the _replicate database:

~~~ {.javascript}
{ 
	'source': 'http://admin:password@localhost:5984/messages', 
	'target': 'http://admin:password@localhost:5984/user1messages'
	'filter': 'filters/userMessageFilter'
	'query_params': '{"user": "user1"}',
	'continuous': true
}
~~~

For the other direction it is equally simple we just have to switch source and target and remove the filter. 

~~~ {.javascript}
{ 
	'source': 'http://admin:password@localhost:5984/user1messages'
	'target': 'http://admin:password@localhost:5984/messages', 
	'continuous': true
}
~~~

If we do that for all users we get the behaviour we wanted, that all users get their messages via replication to and from the central database.

## Conlusion

The last reamining step is to give user1 their access token and their database and you are done. At least for the read access part. What I showed above still allows user1 to write arbitrary documents to user1messages which will be replicated to the central database. This can be handeled with validat_update functions, but that is topic for another blog post, I hope I get to next week.  
