---
title: Getting started with Haskell in 2022
authors: [martinsos]
tags: [haskell]
---

import Link from '@docusaurus/Link';
import useBaseUrl from '@docusaurus/useBaseUrl';
import InBlogCta from './components/InBlogCta';
import WaspIntro from './_wasp-intro.md';
import ImgWithCaption from './components/ImgWithCaption'

Haskell is a unique and beautiful language that is worth learning, if for nothing else, then just for the concepts it introduces, which will expand your view on the programming forever.

However, getting started with it can sometimes be overwhelming, due to multiple popular options for build tools, installers, introductory educational resources, and similar.

That is why I decided to write this very opinionated and practical post that will tell you exactly how to get started with Haskell in 2022 in the most standard / common way, without needing to make any decisions that you are not equiped to make at the moment - instead, focus on enjoying learning Haskell :)!

This post was last updated on Sep 2022.

## Installation

For installing Haskell use [GHCup](https://www.haskell.org/ghcup/#) - universal installer for Haskell - that will install for us everything we need, and then will also help us manage those installations in the future (update, switch versions and similar).
It is great because it is super simple to use, it works the same way on all platforms (Linux, Osx, Windows) and you have one central place to take care of your Haskell installation instead of dealing with OS-specific issues.

Follow instructions at [GHCup](https://www.haskell.org/ghcup/#) to perform the installation of GHCup, and then via it installation of the Haskell Toolchain aka stuff that you need to develop in Haskell.
For curious ones, that "stuff" is:
1. GHC -> Haskell compiler
2. HLS -> Haskell Language Server -> your code editor will use this to provide you great experience while editing Haskell code
3. cabal -> Haskell build tool -> you will use this to organize your Haskell projects, build them, run them, define dependencies, ...
4. Stack -> alternative to cabal, but we will go with cabal as our build tool of choice, so you don't have to install Stack.

Are you done installing GHCup and then GHC, HLS and cabal via it? Ok, you are all set then!

## Editor

HLS (Haskell Language Server), that we installed via GHCup, is the one that brings all the cool IDE features to editors, so as long as your editor has a Haskell language extension that utilizes HLS, you are likely good to go.

Safest bet is to go with Visual Studio Code - it has great [Haskell](https://marketplace.visualstudio.com/items?itemName=haskell.haskell) extension that is all you need and usually works great out of the box.

A lot of Haskell programmers also use Emacs and Vim, so I can confirm they also have good support for Haskell.

## Community

By joining the Haskell community you will find a great place to ask for help and also to learn about new developments in Haskell ecosystem.

I personally prefer [r/haskell](https://www.reddit.com/r/haskell/) -> it tracks all the newest events and no question goes unanswered.

There is also [Haskell Discourse](https://discourse.haskell.org/), where a lot of discussions happen, including the more official ones.

A lot Haskellers are also active on IRC: https://wiki.haskell.org/IRC_channel . I personally found it too complex to use.

Check https://www.haskell.org/community for full list of Haskell communities.

## Learning Haskell

### Beginner journey 

I would recommend starting with a book for beginners, specifically [LYAH](http://learnyouahaskell.com).
LYAH has online version that you can read for free, or you can buy a physical version.

If you don't like LYAH, maybe consider one of these other popular beginner books (none of these are free):
1. [Haskell Programming from first principles](https://haskellbook.com/)
2. [Get Programming with Haskell](https://www.manning.com/books/get-programming-with-haskell) 
3. [Programming in Haskell](https://www.amazon.com/Programming-Haskell-Graham-Hutton/dp/1316626229)

Whatever book you go with, I would recommend not getting stuck for too long on any concepts that are confusing to you, especially those further down the book.
Some concepts will just need some time to click, and you shouldn't expect to grasp it all on the first try.
Whatever you do grasp from that first read will very likely be more than enough for you to get going with the first projects in Haskell,
and then later you can come back to those complex concepts to understand them better.

Also, don't be shy to ask community any questions -> there are many Haskellers out there happy to support you in your learning!

Once you are done with the first pass through the book, I would recommend doing a project or two.
You can do something you come up with, or you can follow one of the books that guide you through it, for example:
1. [Learn Haskell by building a blog generator](https://lhbg-book.link/) -> free, starts from 0 knowledge
2. [The Simple Haskell Handbook](https://marcosampellegrini.com/simple-haskell-book) -> not free, expects you to know basics of Haskell

Once you have more experience with projects, I would recommend reading that beginner book (or another beginner book) again - you can skip the parts you know and this time focus on anything that was confusing before, and this time you will likely have much easier time grasping those harder concepts.

That is it! At this point, you are well equiped to write proper real-world Haskell programs!


### But what about X? Do I need to know that to be a real Haskell programmer?
TODO: Kick this chapter out, it is too much?
TODO: Update this to be more like my previous writing? https://www.reddit.com/r/haskell/comments/uygqto/how_can_i_move_from_a_basic_hello_worldnumber/ia4a6na/?utm_source=reddit&utm_medium=web2x&context=3 .

There are a lot of advanced concepts in Haskell, like monads, typeclasses, lenses, template haskell, monad stack transformers, effect systems, and so on, that are often mentioned all around and it can be hard to judge how much of those one needs to learn in order to be effective in Haskell.

We can separate knowledge of Haskell into two levels:
1. Knowledge that allows you to program at least as effectively and productively in Haskell as you would in some popular (imperative) language. Basically, stuff you need to know to feel comfortable and efficient in Haskell.
2. Knowledge that allows you to go beyond "typical" productivity and really get the most out of Haskell.

I will try to give you a quick, imperfect overview of what is enough to know to gain that first level of Haskell knowledge:
1. Basic syntax, functions, defining data types, dealing with lists, ... -> all the basic stuff that any beginner book will cover in the first half or so.
2. Typeclasses -> using popular ones and creating your own typeclasses.
3. Understanding basics of Monad, using popular Monads, especially IO, but not writing your own Monads.
4. Understanding of Functor and popular Functors.
5. Basic usage of monad stack transformers.

## Super opinionated summary
TODO: Provide links for stuff.
1. Install [GHCup](), then install GHC, HLS and cabal via it.
2. Use vscode with [Haskell]() extension, or maybe emacs/vim.
3. Join [r/haskell](https://www.reddit.com/r/haskell/).
4. Read [LYAH]() book, then [build blog generator in Haskell](). Focus on getting through stuff instead of understanding everything fully.

## FAQ
### cabal vs Stack
There are two popular build tools for Haskell: [cabal](https://cabal.readthedocs.io/) and [Stack](https://docs.haskellstack.org/).
Both are widely used and have their pros and cons, but 

### Do I need to have PhD in category theory to understand Haskell?
TODO
