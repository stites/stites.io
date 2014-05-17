---
layout: post
title: Node Streams
---

{{ page.title }}
================

<p class="meta">16 April 2014 - San Francisco, CA</p>

Node has brought to us the ability to run javascript anywhere with the pretense that asyncronisity is getting our servers comparably as fast as a C++ or Go server. One of the key, unsung features which allows for such amazing speed is concepts of streams.

Buffering will loads files/documents/objecs into memory, however streams can bypass this loading process and allow content to be directly funnelled into a server response, which happens to be a stream object!

Normally, in node, you'd use:


    app.get('/route', function(req, res){
        fs.readFile(__dirname + '/data.txt', function (err, data) {
            res.end(data);
        });
    })


however the asyncroncous `readFile` is a buffer and thus a much speedier options would be:


    app.get('/route', function(req, res){
        var stream = fs.createReadStream(__dirname + '/data.txt');
        stream.pipe(res);
    })

Lets see what node has to offer.

-----------

Node comes with four classes for streams: `stream.Readable`, `stream.Writable`, `stream.Duplex`, and `stream.Transform`. From my perspective, the core functionality of streams breaks down into Readable and Writable streams. They're also very similar, so I'll mostly explain how they work through the example of a Readable one.

### `stream.Readable`
Very simply, Readable streams are read-only and serve you data. But there are two ways this can be done in a 'flowing' state and a 'non-flowing' one. When you're in 'flow' mode, the data is being read as fast as possible and you'll have to use event handlers to process your data and these will be sent to you in chunks. In non-flowing mode, you have to call stream.read() to get those chunks.

### `stream.Writable`
Put most simply, this is the writable version of a stream and data will be written to the destination you indicate.

### `stream.Duplex`
Duplex streams are also, simply, streams that both have read and write functionality.

### `stream.Transform`
Transform streams allow you to both read and write in a stream, however also allow you to operate on the stream before serving the data. An example which will clear up any ambigulity would be node's zlib, where a stream takes some data and zips it to a packaged format - thus serving the user an altered stream.

### Also, `.pipe()` and events!
`.pipe()` is the primary utility for streaming. In a terminal, a pipe looks like `|`. `pipe` is a data flow control function which pulls data out of a stream and allows it to be used by some process on the other end of the pipe. The nifty thing about this is that `pipe` will ensure that the destination is not overwhelemed by a fast stream! You can also chain pipes together to generate compound, complex streams - something taken advantage of in gulp.js!

On top of this, each stream class comes with a handful of events which makes it possible to control the flow of a stream while streaming. What does this mean? Well, in essence what I'm saying is that it wouldn't be too hard to roll out your own implimentation of sockets.io! Since all four stream classes, at their core, are just implimentaitons of readable and writable streams, some of the events you'll come across include Readable events:

+ `readable`
+ `data`
+ `end`
+ `close`
+ `error`

and Writable events:

+ `drain`
+ `end`
+ `finish`
+ `pipe`
+ `unpipe`
+ `error`

------------

### In conclusion:

Start using more streams - it will speed up your applications and they aren't difficult to use or think about! On top of this, streaming is also a powerful tool in terminal commands, so you'll get an added benefit of less overhead mental frameworks when you adopt it!
