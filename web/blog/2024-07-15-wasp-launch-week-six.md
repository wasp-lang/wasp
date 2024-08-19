---
title: 'Wasp Launch Week #6: The Fun Side of Web Development 🕺 🪩'
authors: [matijasos]
image: /img/lw6/lw6-banner.png
tags: [launch-week, update]
---

import Link from '@docusaurus/Link';
import useBaseUrl from '@docusaurus/useBaseUrl';

import InBlogCta from './components/InBlogCta';
import WaspIntro from './_wasp-intro.md';
import ImgWithCaption from './components/ImgWithCaption'

<ImgWithCaption
    alt="Launch Week 6 is here"
    source="img/lw6/lw6-banner.png"
/>

Bonjour Wasp connoisseurs 🐝 👋,

It's been a while, but we're back! We've been busy wasps and put our antennae down to work to deliver you v0.14, which is happening [in exactly two days, on July 17th](https://discord.gg/aMvmMaa5?event=1261263045741510687)!

<ImgWithCaption
    alt="It's happening!"
    source="img/lw6/its-happening.gif"
/>

To reserve your spot (we can fit only so many people in our Discord), click [here](https://discord.gg/aMvmMaa5?event=1261263045741510687).

<ImgWithCaption
    alt="Join the kick-off event"
    source="img/lw6/lw6-join-kickoff.png"
/>

Once you see the invite, mark yourself as "Interested," and that's it (don't make us talk to ourselves again)! Also, if you thought you'd slip by without a bad joke, you were direly wrong:

> Why do wasps never leave tips? Because they are stingy.
> 
> 🤯🤯🤯

Okay, now to the fun stuff—let's see what we're packing into this upcoming release!

## #1: 📝 Define your data models in a separate prisma.schema file!

This change marks the beginning of one of our most requested features - splitting your Wasp config into multiple files! We know that as you develop your Wasp app and it grows, it can become unwieldy to have everything in one hefty .wasp file.

<ImgWithCaption
    alt="Define data models in prisma.schema file"
    source="img/lw6/prisma-split.png"
/>

That's why we started by extracting the data model definitions into a standalone Prisma schema file! This will significantly reduce the size of the Wasp config file and also allow for **a more streamlined experience of writing PSL (Prisma Schema Language), with all the goodies like syntax highlighting and auto-completion working out of the box 🎉**.

## #2: 🔒 Auth Hooks - onBeforeSignup, onAfterSignup, and more! 

<ImgWithCaption
    alt="Auth lifecycle hooks"
    source="img/lw6/auth-hooks.png"
/>

Although Wasp's Auth feature is probably the fastest way to get authentication running in your full-stack app, **adding your custom logic to the auth process can also be quite handy—e.g. if you want to log something, do some extra config etc**.

We've made that easy, by offering several authentication lifecycle hooks that you can use for the exact purpose!

## #3: 🆕 New authentication provider: Discord!

<ImgWithCaption
    alt="Discord as a new auth provider"
    source="img/lw6/auth-discord.png"
/>

This one is pretty self-explanatory, but that doesn't make it any less cool! Besides Google and GitHub, **Discord is now a third social auth method natively supported by Wasp, next to Google and GitHub** - that means all you need to do is define a single line in your Wasp config, and voilà - your users can now sign in with Discord!

## #4: 👀 TypeScript SDK RFC - a sneak peek!

<ImgWithCaption
    alt="TS SDK proposal of code"
    source="img/lw6/ts-sdk.png"
/>

As you might have seen in the community, this has been an ongoing topic of discussion for a while. Although having a dedicated configuration language (DSL) allows for the maximum customizability of the DX, **having Wasp config in TypeScript instead will help out with language tooling (IDE syntax highlighting and auto-completion). Also, it might feel even more familiar to developers using Wasp**.

This is why we decided to test the waters and see how we (and you) like it! We are still working out what it will look like, and we have laid out some of the ideas in this [RFC for TS SDK](https://wasp-lang.notion.site/RFC-TypeScript-DSL-Design-ec5d8c80bffe4fa19537ec659caf5b67).  We'd love to hear from you and get your comments and ideas in this GitHub issue (or just come to [our Discord](https://discord.gg/rzdnErX) and bash us there 😅)

## #5: 🤯 OpenSaaS, Reloaded!

<ImgWithCaption
    alt="Open SaaS banner"
    source="img/lw6/opensaas-banner.png"
/>

And finally, the star of the last launch week, [Open SaaS, a 100% free and open-source boilerplate starter for React & Node.js, powered by Wasp](https://opensaas.sh/), has received its first makeover! You gave us a ton of amazing feedback and ideas, and we listened. Here's what's new:

- Simplified Stripe payment logic
- The code is now organized vertically by features (instead of client and server folders)
- Introduced e2e tests with Playwright
- Added an optional cookie consent banner that hooks up to Google Analytics
- Bunch of small bug fixes and docs updates

And more! Don't forget that Lambo you will earn with your SaaS is just within arm's reach (well, it depends on how long your arm is). All you have to do is go to [OpenSaaS](https://opensaas.sh/) and finally start that app you've been dreaming about  🏎️.

## #6: 🫵 See you there!

That's pretty much it—we've given you a taste of what's coming, but for the real deal, you'll have to join us on Wednesday! Register here and make sure to mark yourself as interested—we'll see you there if you don't see us first (sorry)!

<ImgWithCaption
    alt="Join the kick-off event"
    source="img/lw6/lw6-join-kickoff.png"
/>

Register for the kick-off event [here](https://discord.gg/aMvmMaa5?event=1261263045741510687).

## Stay in the loop

<ImgWithCaption
    alt="dont leave"
    source="img/lw5/dont-leave.gif"
/>

Every day, we'll update this page with the latest announcement of the day - to stay in the loop, [follow us on Twitter/X](https://twitter.com/WaspLang) and [join our Discord](https://discord.gg/rzdnErX) - see you there!