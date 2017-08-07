---
layout: post
title: A useful `==` in Javascript
---

<p class="meta">17 May 2014 - San Francisco, CA</p>

Today I was reviewing John Resig's [Secrets of a Javascript Ninja Quiz][eresig] and noticed in section 21 that, when checking values in a cache, he uses a double-equals sign to see if a value exists on the object. Check it out:

    function isPrime( num ) {
      if ( isPrime.cache[ num ] != null )
        return isPrime.cache[ num ];
      
      var prime = num != 1; // Everything but 1 can be prime
      for ( var i = 2; i < num; i++ ) {
        if ( num % i == 0 ) {
          prime = false;
          break;
        }
      }
     
      isPrime.cache[ num ] = prime
     
      return prime;
    }

At first I thought this was a slip up, however after digging further, I noticed that this loose equality only applies to two objects: the `undefined` object and the `null` object - not all falsy values. While [Dr. Axel Rauschmayer][2ality] paints a clear picture as to why `==` is confusing and we should tend to avoid it, I must admit that Resig's example might be the only place I have seen an example of where it could be considered legitmate: this provides a cleaner way to check for both `undefined` and `null` inputs in one line. While I still don't think that you should ever use `==` in your code for other reasons, I think it's very powerful to see specific cases where it does provide use. [benvie][so_user] provides a very thorough answer on the differences between how `==` and `===` work after digging directly into V8 which you can find on the SO question: [JavaScript performance difference between double equals (`==`) and triple equals (`===`)][so]

[2ality]: http://www.2ality.com/2011/06/javascript-equality.html
[eresig]: http://ejohn.org/apps/learn
[so]: http://stackoverflow.com/questions/8044750/javascript-performance-difference-between-double-equals-and-triple-equals
[so_user]: http://bbenvie.com/
