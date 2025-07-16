---
title: "Wasp: The first full-stack framework powered by an LLM - running on vibes, not a compiler"
authors: [matijasos]
image: /img/wasp-llm/wasp-llm-banner.png
tags: [update]
---

import Link from '@docusaurus/Link';
import useBaseUrl from '@docusaurus/useBaseUrl';

import InBlogCta from './components/InBlogCta';
import WaspIntro from './_wasp-intro.md';
import ImgWithCaption from './components/ImgWithCaption'

**This blog post is an April 1st joke. Everything we said here wasn't true, we're not using LLMs to generate your code. Wasp is a full-stack, batteries-included web framework, that is being developed by humans for humans and AI tools 😃**

For those of you who are new here, we have been building a **full-stack, batteries-included web framework** for the last four years. **You can think of [Wasp](https://github.com/wasp-lang/wasp) as a modern, JS-based incarnation of Laravel, Django or Ruby on Rails**. We based it on a custom compiler so it doesn’t depend on the specific stack or the architecture in the long run (currently it supports React, Node.js and Prisma).

**Wasp just crossed [16,000 stars on GitHub](https://github.com/wasp-lang/wasp)**, with thousands of devs using it both in their startups and enterprises.

## Run on vibes, not the compiler

Still, with the recent developments in the LLM-powered code generation, we realised our current approach simply isn’t feasible anymore. We decided that instead of implementing and maintaning our own compiler, we can simply outsource that to an LLM.

<ImgWithCaption alt="Wasp AI-first diagram" source="img/wasp-llm/wasp-ai-first-diagram.png" caption="Let's get these vibes a'goin'." />

Here's why it makes much more sense and completely changes the game:

## Nobody looks at the generated code anymore

<ImgWithCaption alt="jQuery code example" source="img/wasp-llm/jquery-code-example.png" caption="Let's face it - this is what it's gonna be." />

Gone are the days when we as developers argue about which is the best UI or state management library. No more asking our colleagues or on Reddit "what is everyone using nowadays," just to figure out there's a cool new thing you now have to learn.

All of that is now delegated to the AI — it has been trained on millions of codebases, and it can bring a much more sound decision than we can with our limited experience. If it turns out that its weapon of choice is jQuery or Redux v3.7.0 from 2017, so be it. It's not like anybody is ever going to be looking at the actual code and try to refactor it manually.

## Full-stack goes “fluid” - never get the same app twice

We’re now entering the era of a generative and so-called “fluid” UI. Instead of the traditional hard-coding of UIs for the specific pages (e.g. User profile, Settings, Billing, …), the AI will generate a personalized UI for each user. **We decided to take that one step further -** **not only UI should be customized, but rather all the parts of the stack, including backend logic and the database models.**

That means each time you change or re-deploy your app, it will be slightly different. Sometimes it might be completely unnoticeable change (e.g. a different hashing algorithm or lodash using another version), and sometimes a feature will work a bit differently than the last time a user tried it.

There are two obvious benefits to this:

- **Your UI will always feel “fresh” and keep your users engaged**, making sure they don’t get caught up in a rut.
- **It’s a great security enhancement** - good luck hacking an app that constantly changes it’s implementation

## Prompts are the new code, but with less hassle

<ImgWithCaption alt="prompts > coe" source="img/wasp-llm/give-me-auth.png" caption="Code is so 2024. Embrace the prompt." />

With these fundamental changes, Wasp will move from the classical notion of the framework to **the novel AI-first, prompt-based system for building full-stack experiences**. Wasp will preserve a certain degree of backward compatibility with the previous versions and let developers define their own components, and even the backend logic, but we recommend everyone to start transitioning towards the new paradigm.

**We’ve discovered that prompts are vastly superior to the code** - they don’t have to be versioned nor shared among the team members. Each new team member can simply browse the app and discover its features, instead of reading through all the tedious code and tests.

Lastly, there is no specialized knowledge required - anybody can start contributing right away.

## Thoughts and ideas? We’d like to hear from you!

As always, we’re grateful for your support and would love to hear from you. We realize this is quite a big change but are confident in the new direction we’re taking with Wasp. AI and LLM-powered code generation offers so many new possibilities and it is our responsibility to find the best way to make use of it.

Let us know what you think on [our Discord](https://discord.gg/rzdnErX) - we’re looking forward to seeing you there!
