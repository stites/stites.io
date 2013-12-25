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
First off, I'd like to start off by making a plug for the codebase I've put together. You can find it [here](www.github.com/stites) in [my github](www.github.com/stites) and follow along. if you'd like. There are also 32 test done in BDD methodology. The bloomfilter itself is written in pseudoclassical.

To start, we need five variables on our Pseudoclassical Bloomfilter. The size of our bloom filter, `m`, the number of hash functions in our filter, `k`, some kind of storage for those hash functions, `hashStorage`, the number of items we put in our filter, `n`, and the storage unit itself, `storage`. `m` and `k` are predetermined and the hash functions have to be included in our `hashStorage` before we being. Our `storage` unit and number of items, `n`, also start at `0`. Code:

    var BloomFilter = function (m, k) {
      this._m = m;
      this._k = k;
      this._n = 0;
      this._storage = 0;
      this._hashStorage = Array(k);
    }

We need a way to take an input string and retreive the hash of each hash function. This is going to be used everywhere and will be used as a bitmask to add to our storage. Lets create a function which will generate an array of our desired masks:

    BloomFilter.prototype.getMask = function(strVal) {
      var mask = 0;
      for (var i = 0; i < this._hashStorage.length; i++) {
        if (this._hashStorage[i] === undefined) {
          throw new Error('expected '+this._hashStorage.length+' hash functions');
        }
        mask |= 1 << this._hashStorage[i](strVal, this._m);
      };
      return mask;
    };

Notice how we generate a hash at `this._hashStorage[i](strVal, this._m)` and then create a bitmask with `1 << this._hashStorage[i](strVal, this._m)` and compound each mask using the or-assignment `mask |= `... for each iteration of the `for`-loop. This works just like any `+=` or operator if we were using ints, only we're using bitwise cause we're ballers like that. Let's add a value to our storage:

    BloomFilter.prototype.add = function(strVal) {
      var mask = this.getMask(strVal);
      var check = this._storage;
      this._storage |= mask;
      if (check !== this._storage){
        this._n += 1;
      }
    };

Again, using the `|=` operator just like we would use for a `reduce` operation. To query our storage, we expand our horizons with bit-operators with an `&` operator.

    BloomFilter.prototype.query = function(strVal) {
      var mask = this.getMask(strVal);
      var test = mask&this._storage;
      return (test !== mask) ? false : true;
    };

This is not to be confused with an `&&` used for boolean evaluation. This happens on the bit level so, given a `mask` of `00001010` and a `this._storage` of `01101101` we could visualize this operation as follows:

      00001010
    & 01101101
    ----------
      00001000

These kinds of logic gates are _VERY_ userful for complex, or time-sensitive operations. Keep them in mind in the future!

-------

There you have it! that's all there is to a basic bloom filter. The repo has a few more goodies, though, including the calculation of a falsepositive rates which is dependent on that fifth variable we included in our filter, `n`. The false positive rate turns out to be `(1-e`<sup>`-kn/m`</sup>`)`<sup>`k`</sup>. Here's the implimentation:

    BloomFilter.prototype.fp = function() {
      m = this._m;
      k = this._k;
      n = this._n;
      return Math.pow(1 - Math.pow(1-(1/m), k*n), k);
    };

### Invertible Bloom filters
Next up is invertible bloom filters. All that means is that you are adding removal capability to a bloom filter. The small gotcha, aside from the checks, is that now your bloom filter has two levels of uncertainty: one for falsepositives and one for falsenegatives. More on this later. Most likely after I finish my time at (Hack Reactor)[www.hackreactor.com].