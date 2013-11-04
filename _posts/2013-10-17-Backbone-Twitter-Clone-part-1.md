---
layout: post
title: Building a Backbone Twitter Clone Part 1: Introduction
---

{{ page.title }}
================

I've just finished working through JavascriptIsSexy's [Learn Backbone.js Completely][jsbb] which, in my opinion, is one of the best places to learn javascript on the interwebs. I've also been reading John Resig's [Secrets of the Javascript Ninja][ninja], and have completed half of [Learn Javascript Properly][jsjs]. So I may not be a javascript pro, still I'm gunning to build my first Backbone App. This is going to be a twitter clone, because it seems fairly comprehensive. Feel free to follow me on twitter ([@samstites][twitter]), by the way. you can also find the source of these files over at my github page: [stites/BBTwitterAMD][ghtwitter].

To start with, while I understand that this might seem like a bit of a shortcut, I've decided to use [Yeoman][yio] as my workflow tool. I just rewrote the beginnings of a backbone application four times in the past three days, so I think I'm good on that front. Also Yeoman will be a good introduction to some new tools which will force some good habits (hopefully). 

I want to go with as many best practices as possible, so I've decided to make this Twitter Clone with Asyncronous Module Dependency (AMD) which you learn about in the tail end of JavascriptIsSexy's guide. Looking for the appropriate generator, mrichard's [marionette generator][gen] seems perfect. There are also a lot of new technologies, which I'm going to research and breifly introduce in this blogpost.

### The Marionette Generator Stack
#### Server-Side
###### [Node](http://nodejs.org/)
> Node.js is a platform built on Chrome's JavaScript runtime for easily building fast, scalable network applications. Node.js uses an event-driven, non-blocking I/O model that makes it lightweight and efficient, perfect for data-intensive real-time applications that run across distributed devices.

###### [Express](http://expressjs.com/)
Express is a lightweight server-side framework inspired by Ruby's Sinatra.

###### [Socket IO](http://socket.io/)
Socket.IO is a lightweight Websocket API which simplifies bi-directional communication over HTTP protocols. Thus using websockets we get realtime communication. This one will have to be put on the backburner for a while as the app starts off. Also, right now, I know a bit more about AJAX than websockets, so I think I might navigate away from this for a while. Also, I need some practice with AJAX calls and it seems that [there could be cause to not to use Socket.IO - StackOverflow.com][socketio]

#### Database & Object Data Manager
###### [Mongo](http://www.mongodb.org/)
MongoDB is a NoSQL database management system written in C++, it's great for prototyping as it's document type is BSON. Also, on the note of databases, keep in mind that Mongo is strongly consistent, and has partition tolerance according to [Brewer's CAP Theorem][CAP]. This means that all nodes see data at the same time, and that the system will continue to operate event if errors occur. I really want to get into some more database experimentation later on - I come from an SQL background so NoSQL facinates me.

###### [Mongoose](http://mongoosejs.com)
>Mongoose is a MongoDB object modeling tool, written in JavaScript, designed to work in an asynchronous environment. - StackOverflow

#### Client-Side

###### [Backbone](http://backbonejs.org/)
Our client-side framework, running a very structured and well defined MV* model.
###### [Marionette](http://marionettejs.com/)
A library/plugin for Backbone which allows you to scale out modular, event-driven applications.
###### [jQuery](http://jquery.com/)
The sweetest sugar for DOM Manipuation.
###### [Require](http://requirejs.org/)
Making our application modular and optimized for speed! I read some counter arguments talking about how AMD is usless for javascript, however I respectfully disagree and want to give this a shot!
###### [Handlebars](http://handlebarsjs.com/)
A templating library using mustache templates.
###### [The Require-Handlebars Plugin](https://github.com/SlexAxton/require-handlebars-plugin)
so that we can make our handlebar templates useful.
###### [Bootstrap](http://twitter.github.io/bootstrap)
Cause I'm definitely not a designer! Maybe I'll dabble, one day...
###### [SASS-Bootstrap](https://github.com/thomas-mcdonald/bootstrap-sass)
Useful cause I know a little sass from codeschool!

#### Workflow Tooling

###### [Yeoman](http://yeoman.io/)
A workflow which includes [Yo](https://github.com/yeoman/yo) (to scaffold out applications), [Grunt](http://gruntjs.com/) (to build, preview and test projects), and [Bower](http://bower.io/) (for dependency management).

#### Testing
Okay I'm gunna be honest since this is my first backbone app, testing is a little over my head. Listing these out, however I am not sure I will use them all, but I'd like to give Jasmine a shot.

###### [Jasmine](http://pivotal.github.io/jasmine//)
Swapped out Mocha for this in the generator. Automated testing framework for JavaScript. Want to try and use this.
###### [phantomJS](http://phantomjs.org/)
Is for headless Webkit testing, works with other testing frameworks like Jasmine.
###### [Chai](http://chaijs.com/)
A BDD / TDD assertion library.
###### [Sinon](http://sinonjs.org/)
Standalone test spies, stubs and mocks.

Getting Started
=================================
First fired up Yeoman, swapped out Mocha for Jasmine, removed Socket.IO and we're gold!




[CAP]: https://en.wikipedia.org/wiki/CAP_theorem
[gen]: https://github.com/mrichard/generator-marionette
[ghtwitter]: https://github.com/stites/BBTwitterAMD
[twitter]: https://twitter.com/samstites
[jsjs]: javascriptissexy.com/how-to-learn-javascript-properly/
[ninja]: http://jsninja.com/
[jsbb]: http://javascriptissexy.com/learn-backbone-js-completely/
[yio]: http://yeoman.io/
[socketio]: http://stackoverflow.com/questions/4848642/what-is-the-disadvantage-of-using-websocket-socket-io-where-ajax-will-do