---
layout: post
title: Bloom Filters for Data Detectives
---

{{ page.title }}
================

November 30th, 2013
Track I'm coding to: `Soul Satisfaction` by `Bit Funk` on [hypem](www.hypem.com)

I just finished writing a Bloom filter over Thanksgiving break. What an incredible experience! Aside from learning how to whip up a bloom filter, this was also my first experience in using bitwise operators. I mean I've used bitwise operators before, but not for anything **useful** just little things like using `~~(n)` instead of `Math.floor(n)` for positive numbers, and `x << y` as a replacement for `x * Math.pow( 2, y )`. Using bitwise operators as a means of generating logic gates has been very enlightening - I look forward to using these in the future.

### Bloom Filters 
I guess I should start with what a bloom filter actually is. I'll start off with an example. Imagine you're Sherlock Holmes and you are chasing Professor Moriarty through London. Clearly I'm talking about *Sherlock Holmes: A Game of Shadows* starring you (as Sherlock) and Jude Law as your trusty companion Watson. First of all, you have no idea where Moriarty is - you just know that he's somewhere in London. However, you have done your due-diligence and you've retreived a list of all crimes which may have been linked to Moriarty. Keep in mind that the police have been on top of their work and every crime moriarty has ever commited is **definitely** on the list, no exceptions, however there might be a few extra cases in there. 

What a relief! Instead of having to search through every house in the city, you already have a pre-vetted list of all the crimes Moriarty has ever commited! It's almost as if it came from the hands of Burton Howard Bloom, himself! What we now hold is, essentially, a bloomfilter. Like a bloom filter, this list may include some red herrings - however overall, we've already killed a ton of work by not having to chase through every crime in london.

### Getting a little more technical
Now imagine you have a database. Only some of the information you collect is so vast, that a collection doesn't fit on one physical disk - User Tracking was a biggie back in my TurboSquid days. Instead of having to parse through every single disk, we could run queries much faster if we checked our criteria against a list which told us if what we wanted was on that disk, if it wasn't, we could simply skip over it.

The requirements for such a list would mean that everytime we added an item to the disk:
(a) add some new data structure to our already-complex business logic
(b) write some kind of identifier from each item into that data structure, and
(c) parse through every item in this data structure in order to see if it is in our list.

While there are many ways to impliment this pre-vetting service in your apps, clearly, the best way is to use a bloom filter. Bloom filters - using bitwise operators - are `O(1)`, can take up as much space as a single 32-bit character for storage, and can reduce the number of operations down to as little `O(1)` if your bloom filter indicates what you are looking for does not exist in your data structure.

There is a single gotcha, however. Bloom filters are a probabilistic hash meaning that, while they can tell you with 100% certainty if something is *not* in the filter, they also have a false-positive rate associated with them. This false-positive rate is dependent on how many hash functions you are using, how many items have been put in your filter, and how much size you have alloted your filter to take. Enough details, lets start coding.

### Building one:

