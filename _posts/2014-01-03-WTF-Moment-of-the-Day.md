---
layout: post
title: WTF Moment of the Day
---

{{ page.title }}
================

<p class="meta">03 Jan 2014 - San Fransisco, CA</p>

    var companies = 'abcdefghijklmnopqrstuvwxyz'.split('');
    companies.push('other');

returns:

    [ 'a',
      'b',
      'c',
      'd',
      'e',
      'f',
      'g',
      'h',
      'i',
      'j',
      'k',
      'l',
      'm',
      'n',
      'o',
      'p',
      'q',
      'r',
      's',
      't',
      'u',
      'v',
      'w',
      'x',
      'y',
      'z',
      'other' ]

but

    'abcdefghijklmnopqrstuvwxyz'.split('').push('other');

returns

    27

#### WTF?

Checking out the V8 source code, which can be found [here at code.google](https://code.google.com/p/v8/source/browse), I found it was a little tough to navigate, luckily you can pull down the source code with a quick `git clone git://github.com/v8/v8.git v8 && cd v8`. 144Mb! Daunting! Clearly, we need to find `String.prototype.split` somewhere in this codebase, where to start?

`grep -nr "String.prototype.split" *` has the answers. Aside from the changelog and a number of tests, `src/string.js:614: ["String.prototype.split"]);` is the best lead. Here's the native code:

    // ECMA-262 section 15.5.4.14
    function StringSplit(separator, limit) {
      if (IS_NULL_OR_UNDEFINED(this) && !IS_UNDETECTABLE(this)) {
        throw MakeTypeError("called_on_null_or_undefined",
                            ["String.prototype.split"]);
      }
      var subject = TO_STRING_INLINE(this);
      limit = (IS_UNDEFINED(limit)) ? 0xffffffff : TO_UINT32(limit);

      var length = subject.length;
      if (!IS_REGEXP(separator)) {
        var separator_string = TO_STRING_INLINE(separator);

        if (limit === 0) return [];

        // ECMA-262 says that if separator is undefined, the result should
        // be an array of size 1 containing the entire string.
        if (IS_UNDEFINED(separator)) return [subject];

        var separator_length = separator_string.length;

        // If the separator string is empty then return the elements in the subject.
        if (separator_length === 0) return %StringToArray(subject, limit);

        var result = %StringSplit(subject, separator_string, limit);

        return result;
      }

      if (limit === 0) return [];

      // Separator is a regular expression.
      return StringSplitOnRegExp(subject, separator, limit, length);
    }

Crazy! So this is a pretty neat trip we're on. Some of the code is left to speculation. However it looks pretty straight forward. Next up, it looks like we're preping an array:

    var ArrayPushBuiltin = $Array.prototype.push;

And finally, `StringSplitOnRegExp`:

    function StringSplitOnRegExp(subject, separator, limit, length) {
      %_Log('regexp', 'regexp-split,%0S,%1r', [subject, separator]);

      if (length === 0) {
        if (DoRegExpExec(separator, subject, 0, 0) != null) {
          return [];
        }
        return [subject];
      }

      var currentIndex = 0;
      var startIndex = 0;
      var startMatch = 0;
      var result = [];

      outer_loop:
      while (true) {

        if (startIndex === length) {
          %_CallFunction(result, %_SubString(subject, currentIndex, length),
                         ArrayPushBuiltin);
          break;
        }

        var matchInfo = DoRegExpExec(separator, subject, startIndex);
        if (matchInfo == null || length === (startMatch = matchInfo[CAPTURE0])) {
          %_CallFunction(result, %_SubString(subject, currentIndex, length),
                         ArrayPushBuiltin);
          break;
        }
        var endIndex = matchInfo[CAPTURE1];

        // We ignore a zero-length match at the currentIndex.
        if (startIndex === endIndex && endIndex === currentIndex) {
          startIndex++;
          continue;
        }

        %_CallFunction(result, %_SubString(subject, currentIndex, startMatch),
                       ArrayPushBuiltin);

        if (result.length === limit) break;

        var matchinfo_len = NUMBER_OF_CAPTURES(matchInfo) + REGEXP_FIRST_CAPTURE;
        for (var i = REGEXP_FIRST_CAPTURE + 2; i < matchinfo_len; ) {
          var start = matchInfo[i++];
          var end = matchInfo[i++];
          if (end != -1) {
            %_CallFunction(result, %_SubString(subject, start, end),
                           ArrayPushBuiltin);
          } else {
            %_CallFunction(result, UNDEFINED, ArrayPushBuiltin);
          }
          if (result.length === limit) break outer_loop;
        }

        startIndex = currentIndex = endIndex;
      }
      return result;
    }

This one is going to take some more digging. Stay tuned for a part two on this later.

