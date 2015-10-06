---
layout: post
title: Haskell N-Ary Trees
---

{{ page.title }}
================

<p class="meta">06 Oct 2015 - San Francisco</p>

In the start of my work on CSSR, I have had to construct n-ary trees. Surprisingly,
this took a lot or realization to get to, since most examples in haskell are showered
with binary trees, so it's easier to forget their n-ary variants. Furthermore, most
n-ary implemintations that I've come across don't seem to give nodes any value,
except at the lowest level. Well, that last part may seem a little insane, but
perhaps this was because a majority of my searching was on stackoverflow.

So, in generating a `ParseTree` from Cosma's Causal State Splitting Reconstruction
(CSSR) paper, I devised the following implementation which splits the root of the
tree from its branches. Each branch maintains a tuple with the first value as the
node's weight. Just to start off, a parse tree only contains `Char` values:

    data ParseTree = Root [ParseTreeBranch] deriving Show
    data ParseTreeBranch = Branch (Char, [ParseTreeBranch]) deriving Show

    parseTree = Root [
      Branch ('a', [
        Branch ('b',[
          Branch ('c',[]),
          Branch ('a',[])
        ]),
        Branch ('c',[
          Branch ('c',[]),
          Branch ('a',[])
        ])
      ]) ]

Filling a parse tree is a little more complex, and I will save that for a later
date (once the implimentation is cleaned up a bit). Currently, this is really
just a trie implimentation, but it should expand to take any type of sequence.

