---
title: How to get started with Haskell in 2022 (the straightforward way) 
authors: [martinsos]
image: /img/filip-headshot-min.jpeg
tags: [webdev, haskell, language]
---

import Link from '@docusaurus/Link';
import useBaseUrl from '@docusaurus/useBaseUrl';
import InBlogCta from './components/InBlogCta';
import WaspIntro from './_wasp-intro.md';
import ImgWithCaption from './components/ImgWithCaption'


Haskell is a unique and beautiful language that is worth learning, if for nothing else, then just for the concepts it introduces. They will expand your view on programming.

I have been programming in Haskell on and off since 2011 and professionally for the past 2 years, building a [compiler](https://github.com/wasp-lang/wasp). While in that time Haskell has become much more beginner-friendly, I keep seeing beginners who are overwhelmed by numerous popular options for build tools, installers, introductory educational resources, and similar. [Haskell’s homepage](https://www.haskell.org/) getting a call from the previous decade to give them their UX back also doesn’t help. 

That is why I decided to write this opinionated and practical post that will tell you exactly **how to get started with Haskell in 2022 in the most standard / common way.** Instead of worrying about decisions that you are not equipped to make at the moment (like “what is the best build tool Ifd??”), you can focus on enjoying learning Haskell :)!

<!--truncate-->

## TLDR / Super opinionated summary

1. For setup, use [GHCup](https://www.haskell.org/ghcup/). Install GHC, HLS, and cabal.
2. As a build tool, use [cabal](https://cabal.readthedocs.io/).
3. For editor, use VS Code with [Haskell extension](https://marketplace.visualstudio.com/items?itemName=haskell.haskell). Or, use emacs/vim/....
4. Join [r/haskell](https://www.reddit.com/r/haskell/). Feel free to ask for help!
5. To learn the basics of Haskell, read the [LYAH](http://learnyouahaskell.com/) book and [build a blog generator in Haskell](https://lhbg-book.link/). Focus on getting through stuff instead of understanding everything fully; you will come back to it later again.

## 1. Setup: Use GHCup for seamless installation

[GHCup](https://www.haskell.org/ghcup/#) is a universal installer for Haskell. It will install everything you need to program in Haskell and will help you manage those installations in the future (update, switch versions, and similar). It is simple to use and works the same way on Linux, macOS, and Windows. It gives you a single central place/method to take care of your Haskell installation so that you don’t have to deal with OS-specific issues.

To install it, follow instructions at [GHCup](https://www.haskell.org/ghcup/#). Then, use it to install the Haskell Toolchain (aka stuff that you need to program in Haskell). 

Haskell Toolchain consists of:

1. GHC -> Haskell compiler
2. HLS -> Haskell Language Server -> your code editor will use this to provide you with a great experience while editing Haskell code
3. cabal -> Haskell build tool -> you will use this to organize your Haskell projects, build them, run them, define dependencies, etc.
4. Stack -> cabal alternative, which you won’t need for now since we’ll go with cabal as our build tool of choice

## 2. Build tool: Use cabal

There are two popular build tools for Haskell: [cabal](https://cabal.readthedocs.io/) and [Stack](https://docs.haskellstack.org/). Both are widely used and have their pros and cons. So, one of the hard choices beginners often face is which one to use.

Some time ago, cabal was somewhat hard to use (complex, “dependency hell”). That’s why Stack was created: a user-friendly build tool that solves some of the common issues of cabal. (Interestingly, Stack uses cabal’s core library as its backend!) However, as Stack was being developed, cabal advanced, too. Many of its issues have been solved, making it a viable choice for beginners.

In 2022, I recommend `cabal` to beginners. I find it a bit easier to understand when starting out (no resolvers), it works well out of the box with GHCup and the rest of the ecosystem, and it seems to be better maintained lately. 

## 3. Editor: VS Code is a safe bet

HLS (Haskell Language Server) brings all the cool IDE features to your editor. So, as long as your editor has a decent Haskell language extension that utilizes HLS, you are good.

The safest bet is to go with **Visual Studio Code** — it has a great [Haskell extension](https://marketplace.visualstudio.com/items?itemName=haskell.haskell) that usually works out of the box. A lot of Haskell programmers also use Emacs and Vim. I can confirm they also have good support for Haskell. 

## 4. Community: r/haskell and more

Haskell community is a great place to ask for help and learn about new developments in the  ecosystem. I prefer [r/haskell](https://www.reddit.com/r/haskell/) -> it tracks all the newest events and no question goes unanswered. There is also [Haskell Discourse](https://discourse.haskell.org/), where a lot of discussions happen, including the more official ones. A lot of Haskellers are still active on [IRC](https://wiki.haskell.org/IRC_channel), but I find it too complex and outdated to use.

*Check [https://www.haskell.org/community](https://www.haskell.org/community) for a full list of Haskell communities.*

## 5. Learning: You don’t need a math degree, just grab a book

There is a common myth going around that you need a special knowledge of math (PhD in category theory!) to be able to program in Haskell properly. From my experience, this is as far from the truth as it can be. It is certainly not needed, and I seriously doubt it helps even if you have it. Maybe for some very advanced Haskell stuff, but certainly not for junior/intermediate level.

Instead, learning Haskell is the same as learning other languages -> you need a healthy mix of theory and practice. The main difference is that there will be more unusual/new concepts than you are used to, which will require some additional effort. But these new concepts are also what makes learning Haskell so fun!

I recommend starting with a book for beginners, [LYAH](http://learnyouahaskell.com/). It has an online version that you can read for free, or you can buy a printed version if you like physical books.

If you don't like LYAH, consider other popular books for beginners (none of them are free though):

1. [Haskell Programming from first principles](https://haskellbook.com/)
2. [Get Programming with Haskell](https://www.manning.com/books/get-programming-with-haskell)
3. [Programming in Haskell](https://www.amazon.com/Programming-Haskell-Graham-Hutton/dp/1316626229)

Whatever book you go with, don’t get stuck for too long on concepts that are confusing to you, especially towards the end of the book. Some concepts will just need time to click; don’t expect to grasp it all on the first try. Whatever you do grasp from the first read will likely be more than enough to get going with your first projects in Haskell. You can always come back to those complex concepts later and understand them better. Also, don’t be shy to ask the community -> there are many Haskellers out there happy to support you in your learning!

Once you take the first pass through the book, I recommend doing a project or two. You can come up with an idea yourself, or you can follow one of the books that guide you through it. 

For example:

1. [Learn Haskell by building a blog generator](https://lhbg-book.link/) -> free, starts from 0 knowledge, and could even be used as the very first resource
2. [The Simple Haskell Handbook](https://marcosampellegrini.com/simple-haskell-book) -> not free, expects you to know the basics of Haskell already

Once you have more experience with projects, I would recommend re-reading your beginner book of choice. This time, you can skip the parts you already know and focus on what was confusing before. You will likely have a much easier time grasping those harder concepts.

p.s. If you are looking for a bit of extra motivation, check the blog post my teammate Shayne recently wrote about [his journey with Haskell](https://wasp-lang.dev/blog/2022/08/26/how-and-why-i-got-started-with-haskell). He started in late 2021 and has already made huge progress!

---

*Good luck with Haskell! If you have Haskell questions for me or the rest of the Wasp team, drop me a line at `“martin” ++ “@” ++ concat [”wasp”, “-”, “lang”] <> “.dev”` , or write to #haskell channel in [Wasp-lang Discord server](https://discord.gg/rzdnErX).*