---
layout: post
title: Backbone to Angular Part II
---

{{ page.title }}
================

<p class="meta">17 June 2014 - Redwood City, CA</p>

Lets talk about tests. Yes, tests. Some of you might love the phrase, and some of
you may hate it. When it comes to refactoring, however, if you're not using tests
you're pretty much going to screw yourself somewhere along the road. Even if
you're not a huge fan of TDD, doing test-driven refactors are going to save you
a lot of hair pulling down the road. Lets get started.

#### Tools for tests

Angular was built with the idea that testing needed to be an integral part of
the framework. Nothing is merged into core without having tests to go with it.
This is imperative for dynamically-typed functions, like javascript, if you want
to save yourself the overhead of error checking everything under the sun in your
actual codebase.

We have two primary types of tests and test-runners that the angular team
recommends. Karma and Protractor. Both are nessecary: Karma for unit-tests, and
karma for end-to-end tests. First, lets get started with Karma and unit-testing.

#### Setting up Karma

If you don't have karma already in your `package.json` run:

    npm install --save-dev karma

then move into where you'd like to keep all of your tests. For this example,
we'll follow what they do in [`angular-seed`][seed] and place a sibling
**tests** folder _on the same level_ as the client-facing **app** folder.

cd into `tests` and run `karma init karma.conf.js` to generate our config file.
It'll look something like the following. I'll leave it up to you to check out
[karma's documentation on the settings you can configure here][karma.conf].

    // Karma configuration
    module.exports = function(config) {
      config.set({
        // base path, that will be used to resolve files and exclude
        basePath: '',

        // testing framework to use (jasmine/mocha/qunit/...)
        frameworks: ['jasmine'],

        // list of files / patterns to load in the browser
        files: [
          'app/components/angular/angular.js',
          'app/components/angular-mocks/angular-mocks.js',
          'app/scripts/**/*.js',
          'test/spec/**/*.js'
        ],

        // list of files / patterns to exclude
        exclude: [],

        // web server port
        port: 8080,

        // level of logging
        // possible values: LOG_DISABLE || LOG_ERROR || LOG_WARN || LOG_INFO || LOG_DEBUG
        logLevel: config.LOG_INFO,

        // enable / disable watching file and executing tests whenever any file changes
        autoWatch: false,

        // Start these browsers, currently available:
        // - Chrome
        // - ChromeCanary
        // - Firefox
        // - Opera
        // - Safari (only Mac)
        // - PhantomJS
        // - IE (only Windows)
        browsers: ['Chrome'],


        // Continuous Integration mode
        // if true, it capture browsers, run tests and exit
        singleRun: false
      });
    };

with everything in place, `karma start karma.conf.js` kicks off the test runner
and we are good to go.

##### Unit Testing

I'm intentionally glossing over the karma stuff because I want to focus on a few
use cases on the testing itself. Remember

[seed]: www.github.com/angular/angular-seed
[karma.conf]:
