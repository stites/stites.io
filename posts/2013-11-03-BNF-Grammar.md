---
layout: post
title: Backus-Naur Form Grammar
location: San Francisco
---

The latest thing I've done was construct a recursive-decent parser for JSON. The structure of constructing one of these uses a form called [Backus-Naur Form](https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_Form), developed by Backus and Naur, however it is practically identical to [Chomsky's Universal Theory of Grammar][1]. Both have the same format, were developed at the same time, and were developed independently! Great minds think alike!

It's pretty cool to look into, in short the idea is to break down larger structures - sentences - into 'tokens' - nouns, verbs, and objects - and recursively go over the rules you define to generate something meaningful. English example: a sentence is a noun-verb-object structure, so "the rat ate the cheese" is valid. You can recursively iterate the "noun-verb" part to get more meaningful sentences: "the cat ate the rat that ate the cheese" etc.

-----------------------------------------------------

Tangible life lesson. This happens everyday in a lot of respects, sometimes I wind up "recursively" at the grocery store, buying snacks even if I still have to go grocery shopping for dinner later. I wonder where else this takes place. Also when can the iterations be cut back and when are the extra iterations important? **Action item: work on mindfulness.**

[1]: https://en.wikipedia.org/wiki/Universal_grammar
