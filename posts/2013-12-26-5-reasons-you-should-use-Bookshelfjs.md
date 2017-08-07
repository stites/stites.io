---
layout: post
title: 5 reasons you should use Bookshelf.js
location: Washington, DC
---

Back home for the holidays! Of late, I've gotten ahold of my first contract job which I am very excited about. There isn't too much time to look into certain technologies that I'm really stoked about - distributed systems, more work with graphs, or writing a Restricted Boltzman Machine in Javascript - however I am eager to hammer out confidence in client work. One thing that did have me particularly down was that, due to the nature of the app, we'd be working with SQL.

All of last year, I've been an SQL (T-SQL to be precise) fanboy. There is so much data at Turbosquid that it's really a wonderful playground for anyone interested in developing skills in Data Science. However once I discovered the world of databases with no joins, I became feverishly intruiged. At Hack Reactor, you learn relational database architecture from the best, as well as ORMs to mitigate the pain for those new to SQL. Of the choices, I heard some good things about [Bookshelf.js][bookshelf] and I'd like to relate the reasons as to what makes Bookshelf.js stand out as the best Javascript ORM for SQL out there. I'll get started with the reasons why bookshelf is amazing:

#### 1) Bookshelf is a promise based

Okay this one is a little bias: when working in Node, I find that my async calls in other SQL ORMs get crazily nested. I think it's pretty ugly, not readable, and a pain in the ass to keep track of what is gettering called when. I'm sure there are some libraries out there that help mitigate my pains, [async-waterfall][waterfall] - I've heard - is a great way to keep things in order. That aside, when it comes to promises vs. callbacks, there is also a lot of good to be said for promises keeping your code loosely coupled. For a full look at the differences between the two, check out [this stackoverflow question][promises] for starters.

#### 2) Bookshelf is practically Backbone

Here's an example of how I would imagine a classical user would be made in Backbone.js:

    var User = Backbone.Model.extend({
      defaults: {
        'firstName': '',
        'lastName': '',
      }
    });

    var fred = new User({
      firstName: 'Fred',
      lastName: 'Zirdung',
    });

    fred.doSomething();

No imagine you had a preset object, let's call it `Users` which held all of those default values for you:

    var Users = {
      'firstName': '',
      'lastName': '',
    };

    var User = Backbone.Model.extend({
      defaults: Users
    });

    var fred = new User({
      firstName: 'Fred',
      lastName: 'Zirdung',
    });

    fred.doSomething();

Let's assume we're using a module pattern in our code - we want to work in node, of course - so let's pull out Users into the module's global scope:

    // `Users` placed outside this logic

    var User = Backbone.Model.extend({
      defaults: Users
    });

    var fred = new User({
      firstName: 'Fred',
      lastName: 'Zirdung',
    });

    fred.doSomething();

Honestly, we want databases! not objects. It makes sense that our `Users` object should reference some table which we'd have connected to beforehand. Let's swap out `defaults` with a property called `tableName` to make this explicit:

    var User = Backbone.Model.extend({
      tableName: Users
    });

    var fred = new User({
      firstName: 'Fred',
      lastName: 'Zirdung',
    });

    fred.doSomething();

At this point the cat's out of the bag. `Bookshelf` references those tables by some string, so using the `Bookshelf` object connects to our database and we'll have to reference the `tableName` by string.

    var User = Bookshelf.Model.extend({
      tableName: 'Users',
    });

    var fred = new User({
      firstName: 'Fred',
      lastName: 'Zirdung',
    });

    fred.doSomething();

But once we've added Fred's information on the server-side, we still need to send it back to the database. at it's simplist:

    fred.save();

Bookshelf.js' Models and Collections make it so simple to use if you come from a backround in Backbone and don't want to waste your time learning about relational databases.

#### 3) Support for dynamic one-to-one, one-to-many, and many-to-many relations.

Support for one-to-one, one-to-many, and many-to-many relationships should be standard in any decent SQL ORM. Bookshelf's solution is standard with functions like `hasOne`, `hasMany`, `belongsTo`, and `belongsToMany`. However it takes a step forward with it's `through` method. I have less experience with this so I'll just pull a quote from their main site:

> The `through` method helps to create dynamic relations between models & collections, where a `hasOne`, `hasMany`, `belongsTo`, and `belongsToMany` relation may run through a __JoinModel__.

Dynamicly creating relationships? Sounds baller.

#### 4) Supports Knex.js
Bookshelf is also built on Knex, a fantastic query builder. So if you think that there is something that Bookshelf is missing and you don't have time for questions or features being built, build a custom query with Knex.js: you'll have the ability to chain methods like `.select`, `.column`, `.where`, `.from`, and much more. You no longer have to craft massive, multi-line strings and send them straight to your database.

You can access knex through `Bookshelf.knex`. There's also a reference specifically to `Knex.transaction` via `Bookshelf.transaction`.

#### 5) You can build schemas
Finally, you _can_ build schemas in Bookshelf. That took me a bit of digging to find, however using the `Bookshelf.knex` reference, you have all access to Knex's schema building. I realize this is more of a Knex feature, however I figured I could spare someone some digging with this fact.

Some other points of note: Knex also supports foreign keys, cascading deletes, and everything you'll ever need for schema building. Seems like a given, however this feature is undocumented as of 12/26/2013 and the only place where you can find this is in the issues list: [Foreign Key][foreign keys]. You don't even need to use the keyword `foreign`. Here's a merge of some code snippets in the issue:

    return bookshelf.Knex.Schema.createTable("Sessions", function(table) {
      table.increments("id");
      table.dateTime("loggedInAt");
      table.dateTime("lastRequestAt");
      table.integer('userId')
        .unsigned()
        .references('id')
        .inTable('Users')
        .onDelete('CASCADE')  // optional
        .onUpdate(...) // optional
    });

---------------

Bookshelf is an amazing ORM. My experience with it is young, however so far it's been the most fluid ORM I've come across. Coming from a background in Backbone, the only real snag I've expereinced was with schema building and the foreign key issue. If you're interested in learning more feel free to head over to [Bookshelf's mainpage][bookshelf]. Also check out [Knex.js][knex] while you're at it! If you are interested in design patterns, I'd also recommend taking a look at the [data mapper pattern][data mapper pattern] which is what Bookshelf is based off of.

[promises]: http://stackoverflow.com/questions/6801283/what-are-the-differences-between-deferred-promise-and-future-in-javascript
[waterfall]: https://github.com/es128/async-waterfall
[knex]: http://knexjs.org/
[bookshelf]: http://bookshelfjs.org/
[data mapper pattern]: http://en.wikipedia.org/wiki/Data_mapper_pattern
[foreign keys]: https://github.com/tgriesser/knex/issues/24
