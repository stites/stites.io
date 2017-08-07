---
layout: post
title: Moving from Backbone to Angular Part I
---

<p class="meta">11 June 2014 - Redwood City, CA</p>

At bina, we have two products: bina portal and bina annotation platform. While the annotation platform has been written from the ground-up in angular, the bina portal was written in backbone. It's also one of those applications which has grown into a monolithic zombie over the years. It's got to go, but it can't be canned. Taking on the codebase headfirst isn't really an option for us as it means that we will have a brand-new product which won't be user-tested. I'm sure many of you are looking at similar issues. The approach we're taking at bina is to have angular and backbone coexist -backbone is a library, not a framework, afterall- and then slowly have angular take over. Let's get started.

### Coexisting with Require.js

I'm over require.js. It had it's glory days, but it's been on its way out since browserify, it makes for massive code, and is just an overall pain. I'm not saying switch to browserify (yet), but we have a ton of require.js in our code and we don't really want it anymore. How to solve this problem: just put angular in with some good old-fashioned script tags in global scope and declare all of the script tags as you would with a normal application. After you have figured out how you want to structure your application (we've chosen google's recommended [file structure](http://blog.angularjs.org/2014/02/an-angularjs-style-guide-and-best.html)), add them into your project as desired - but before we add angular to our DOM elements, we need to take a closer look at `bootstrap`.

### Rebuilding from the inside-out, feature-first

We're building angular from the inside-out. Our first feature on the list is a pretty complex modal/lightbox which allows a user to navigate through a shared network file storage. First things first, get rid of all that Backbone code. Well, just enough so that it only serves up our partial HTML element. Here you should actually start building your feature only _don't declare any `ng-app`_ instead we're going to bootstrap our application manually. Add in `<script>angular.bootstrap(document, ['myApp']);</script>` at the end of this partial and watch angular kick off as we want!

### A closer look at `angular.bootstrap`

If you're declaring an angular application the normal way, you probably have an `ng-app` somewhere at the top of the page. Well, when angular reads something like `<html ng-app="myApp">` it will see that attribute and immediately start loading as soon as the `DOMContentLoaded` event fires (unless you're working with legacy IE). We don't want that though, so instead there are a few other ways we can kick off angular, manually. We can either invoke bootstrap by initiating the app on the `document` (as suggeted in the previous section) like so:

        angular.bootstrap(document, ['myApp']);

or invoke it on a more specific element using angular's `element` selector:

        angular.bootstrap(angular.element('body')[0], ['myApp']);

Similarly, in the Angular documentation there is the following code example:

        angular.element(document).ready(function() {
            angular.bootstrap(document);
        });

This does something similar to a `$(document).ready` function in jquery, which is nothing new. The usage of `bootstrap` is misleading, however, since there is no app being invoked. This will __only kickoff angular__ but __not any angular modules/application you have declared__. Be careful of this!

### Bootstrapping twice and what's coming up

It's important to also note that if you declare both a `<script>angular.bootstrap(document, ['myApp']);</script>` as well as something similar to `<html ng-app="myApp">` angular will error, claiming that `myApp` has already been bootstrapped. This is because, as mentioned, as soon as `<html ng-app="myApp">` is read, angular will kick off. So there is no need to declare your application anywhere! Next up will be moving from modal to an actual page-view and setting up `ui-router` with lightboxes!

