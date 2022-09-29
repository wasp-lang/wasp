---
title: How and why I got started with Haskell
authors: [shayneczyzewski]
image: /img/filip-headshot-min.jpeg
tags: [webdev, haskell, language]
---

import Link from '@docusaurus/Link';
import useBaseUrl from '@docusaurus/useBaseUrl';
import InBlogCta from './components/InBlogCta';
import WaspIntro from './_wasp-intro.md';
import ImgWithCaption from './components/ImgWithCaption'


I have been programming professionally for over a decade, using a variety of languages day-to-day including Ada, C, Java, Ruby, Elixir, and JavaScript. I’ve also tried some obscure ones, albeit less frequently and for different purposes: MIPS assembly language and OCaml for academic work (I’m a BS, MS, and PhD dropout in CS), and Zig for some side projects. In short, I like learning new languages (at least at a surface level) and have been exposed to different programming paradigms, including functional.

Yet, I have never done Haskell. I’ve wanted to learn it since my college days, but never got the time. In late 2021, though, my curiosity took over. I wanted to see for myself if the mystique and the Kool-Aid hype (or hate) around it are justified. :P So, I decided I’d start learning it on the side and also look for a company that uses it as my next gig. That’s how my Haskell journey started, and [how I got into Wasp](https://wasp-lang.dev/blog/2021/12/21/shayne-intro) a few months later.

<!--truncate-->

## Why learn Haskell?

Haskell seems to have an aura of superiority around it. Many niche and heavily academically-inspired languages do. These languages seem to be used by the enlightened minds and allow you to quickly write complex programs in a fraction of the time with significantly less code. Lisp is amongst these languages, too. Yet, nobody uses them for anything real — only toy projects. (While stroking their long, grey beards under a tree, ruminating on the philosophy of computer science.) At least, that’s the impression I got in college and at work. So, what makes Haskell interesting to learn, let alone want to use professionally?

**First, it is functional as it gets.** While I have used lambdas and functional concepts like `map` in non-functional languages, the fact that these were my *only* choice was really interesting to me. After years of extensive OO usage, I’ve come to appreciate this epigram by Alan Perlis. I think it captures a mindset shift between the two paradigms:

> “It is better to have 100 functions operate on one data structure than 10 functions on 10 data structures.” — Alan Perlis
>

In OO, you create lots of classes with lots of methods. In functional, you have far fewer data structures (mostly list) with a lot more functions. So basically more functions to operate on fewer nouns, whereas OO is lots of nouns, each with many bespoke methods. (The first comment on [this Stack Overflow thread](https://stackoverflow.com/questions/6016271/why-is-it-better-to-have-100-functions-operate-on-one-data-structure-than-10-fun) explains it really well.)

Besides, I liked the idea of referential transparency when writing pure functions. It means that you get the same result back every time you invoke a function, without fear of unknown side effects. (But the language does offer the flexibility to have side effects like IO, via Monads.) I also liked having only immutable data structures — they make reasoning about the system and data flow easier. There were many things like these two that I liked. The point is that thinking functionally really changes the way you structure and solve problems, so I was curious to give it a go.

**Second, Haskell is lazy.** While there are pros and cons to this, it feels undeniably different. Most languages are strict, in that all function arguments are evaluated before invoking a function. This is required because of side effects; to have some expectations regarding the order in which things will run. Haskell does the opposite: it delays evaluation until it’s actually needed.

One contrived yet helpful example of laziness is infinite data structures. Below, we define `fibs` as an infinite `List` of `Integer` values, by using references to *itself*! (You can find a runnable example [here](https://replit.com/@ShayneCzyzewsk1/LazyHaskellExample?v=1#Main.hs).) 

```haskell
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

take 10 fibs -- [0,1,1,2,3,5,8,13,21,34]
```

There’s a downside to laziness, too. It makes it harder to reason about performance and resource utilization. But the idea that you can define things in a declarative way but know that they are evaluated only when needed is a pretty eye-opening way to program.

To sum up: Haskell is functional, lazy, and strongly statically typed. Just the trifecta that gets me out of bed in the morning! :D So, how did I go about learning it?

## Hello Haskell!

I started by reading the canonical Haskell newbie resource, “*[Learn You a Haskell for Great Good!](http://learnyouahaskell.com/),*” often abbreviated LYAH. It was very entertaining, and I learned a lot from it. At times, I wanted it to get to the point more quickly. Still, despite the amusing images and often lengthy examples, it provided me with a great conceptual foundation. I highly recommend it as your first read — it is a really well-written resource for beginners.

After I was about 80% done with LYAH, I switched to a more recent but still popular book: “*[Haskell Programming from First Principles](https://haskellbook.com/)*.” I liked that it started with fundamentals and then moved to more complex topics, slowly but steadily developing my understanding. It was pretty long, though, and sometimes went too far into the weeds. It also had a tinge of intellectual flexing at certain points. Still, it was a good read. I’d read it again if I were starting over.

I also tried [a Haskell course from Google](https://github.com/google/haskell-trainings). Despite being brief, it explains the key concepts in a relatively complete way. If videos are your thing, it might be a solid way to get up to speed.

In short, skimming an intro book to get your foundation solid would be the best bet. I’d also recommend trying out many different online resources when covering more intermediate topics, like Monad Transformers, for example. And don’t worry if it takes a while to start feeling comfortable with things that are pretty specific to Haskell! It just takes some time, and often it is more confusing to derive/deeply understand than to just start using them at first. The understanding will come over time. (Of course, sometimes [pictures](https://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html) help!)

## Setup and IDE support

Getting Haskell up and running was surprisingly straightforward, even though I ran it on an M1 MacBook Air, which was considered a pretty new architecture in 2021. Since the entire toolchain was not fully ARM-compatible back then, some of the setup advice required a bit of modification. But that was no big deal: I used `ghcup`, installed HLS in VS Code, and bam! — I had Haskell up and running. It was a pretty nice experience. 

Some minor downsides I recall:

- There doesn’t seem to be a consensus on which build and package management tool to use, Cabal or Stack. However, unless you’re doing something super specific, it’s not an irreversible decision. At Wasp, we started with Stack but then migrated to Cabal since it better fit our setup and workflows. It was pretty seamless.
- One thing I do miss from other IDEs is breakpoint debugging. Technically, there’s *some* support for it in Haskell, but I don’t think many use it. Breakpoints and lazy evaluation don’t seem to be BFFs.

## 0-60 at work

For someone with experience in several different languages, it is pretty achievable to be able to solve minor bugs/features in Haskell after a few weeks of learning. At least, it was for me. I certainly struggled on best practices and such, and my code reviews involved some Haskell golfing comments for sure :) But I could make it do what I wanted it to do from the functionality perspective. Kudos to the mostly helpful compiler errors (with a bit of practice reading) and the Internet!

Hopefully, your code base demonstrates established project and Haskell patterns, so you can learn as you poke around, and your early code reviewers are supportive coworkers who can explain things as part of their suggestions. I was quite fortunate in that regard: the Wasp team values teaching and learning, and the codebase uses what is called “Simple Haskell”, which limits the use of excessive language extensions in the hopes to keep the core language and concepts as tight as possible. (Note: there are Haskell experts who view this as a severe limitation of the capabilities of the language, but as a newbie, I was happy they did it.)

## So, was the juice worth the squeeze?

Learning Haskell took considerable time and effort. It was completely different from any language I had used before. Yet, I am very happy I embarked on this journey. Even if you do not intend to get a job using Haskell, I still think learning it is worthwhile just to expand your programming point of view and master functional concepts. And for a select set of project types (like writing a compiler for a full-stack web DSL), I feel it really will make you more productive over time. Give an intro to Haskell tutorial or video a try some weekend and let me know what you think! I’m at shayne at wasp-lang dot dev dot com.