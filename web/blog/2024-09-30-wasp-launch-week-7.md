---
title: 'Wasp Launch Week #7: Modern Times ⚙️'
authors: [matijasos]
image: /img/lw7/lw7-banner.png
tags: [launch-week, update]
---

import Link from '@docusaurus/Link';
import useBaseUrl from '@docusaurus/useBaseUrl';

import InBlogCta from './components/InBlogCta';
import WaspIntro from './_wasp-intro.md';
import ImgWithCaption from './components/ImgWithCaption'

<ImgWithCaption
    alt="Launch Week 7 is here"
    source="img/lw7/lw7-banner.png"
/>


Hey Wasp lovers 🐝 💛,

as Charlie Chaplin would say - I'll be back! So are we - despite the vacation season and all the sunshine that kept distracting us, we kept ourselves busy. We closed the curtains and cranked those A/C's on, and brought to you a whole new release of Wasp, as fresh as a cucumber!

<ImgWithCaption
    alt="Charnold"
    source="img/lw7/charnold.png"
    caption="You see? I wasn't lying!"
/>

As always, we’ll present the fruits of our labour in a community call that will happen in exactly three days, on Monday, October 7th, 10.30 AM EDT / 4.30 PM CET! To reserve your spot, [visit the event in our Discord server](https://discord.gg/K4dEEvyR?event=1291419619948626040) and mark yourself as interested.

<ImgWithCaption
    alt="Join the kick-off event"
    source="img/lw7/lw7-discord-event.png"
/>

Do it now, so we have the whole weekend of looking forward to seeing you there! 🐝

With a mandatory admin stuff and a bad joke (Charnold mashup above ICYMI) out of the way, let’s get to the “meaty” stuff and see what is this launch really all about!

## Why Modern Times?

Wasp couldn’t do its magic as well as it does without all the brilliant parts of the stack it uses under the hood - Prisma, React Router, TanStack Query, and many others. That means we also have to keep track of how these tools evolve and update their versions in Wasp (aka make them modern) - and that's exactly what we did for this release!

This launch week isn't so much about introducing new, flashy features (although we have some, and yes, it's TypeScript SDK, and yes, you can try it out), but rather about making what we have better and more up-to-date.

Let's take a look together at the nice things that v0.15 brings us:

## #1: Level up ⬆️: Prisma 5, React Router 6, Express.js

<ImgWithCaption
    alt="level up"
    source="img/lw7/level-up.webp"
    caption="What you will feel like wielding the latest Wasp release (just imagine the flames are yellow)."
/>

As mentioned above, we got to keep up with the trends! Here's what's new:
- **Upgraded Prisma to v5** - no major interface changes, but brings a lot of performance improvements which also make Wasp faster by default 🏎️
- **React Router is now at v6** - v6 has been around for a while (you can see the main changes from v5 [here](https://blog.saeloun.com/2021/12/02/new-features-in-react-router-6/)) and it made the code even more compact and elegant.
- **We cleaned up and bumped a lot of other dependencies** - including Express.js on which we base Wasp’s API layer and our type-safe RPC.

## #2: TS SDK early preview 🤩 - give it a spin and let us know what you think!

During the last launch week we gave you a hint of how the TS SDK for Wasp might look like, and the response we got from you was amazing! This is probably a single feature that has caused the most excitement in the community so far.

<ImgWithCaption
    alt="excited"
    source="img/lw7/excited.webp"
    caption="Yep, that's right"
/>

Motived by your response, we decided to keep the train going. We took your feedback and implemented the first version of TS SDK for you to actually try out!

<ImgWithCaption
    alt="ts sdk code example"
    source="img/lw7/ts-sdk.png"
/>

This is still work in progress but things seem to be coming together quite nicely! We'd also be grateful to hear your thoughts on it once it is out.

## #3: MAGE now uses GPT-4o 🧠 🤖

<ImgWithCaption
    alt="upgrading me"
    source="img/lw7/upgrading-me.webp"
/>

Your favorite [AI-powered SaaS boilerplate starter, MAGE](https://usemage.ai/), has also received an update and now uses OpenAI’s latest model, GPT-4o.

A quick refresher -  we released MAGE about a year ago as a convenient way to kickstart your SaaS powered by React, Node.js, Tailwind, and of course Wasp. It’s been used to generate over 40,000 codebases and is still used daily by developers.

Read [more about it](https://dev.to/wasp/gpt-web-app-generator-let-ai-create-a-full-stack-react-nodejs-codebase-based-on-your-description-2g39) or go ahead and [give it a spin](https://usemage.ai/) - it's 100% free!

## #4: Hackathon incoming 💻 🍪

It’s been a while since we had our last hackathon, and we decided it’s about time we fixed that! For this one, we prepared a special treat for you - I don’t want to spoil too much in advance, but let’s just say it’s going to do something with cookie banners and how to make them as annoying as possible (whoops 🫢).

Here’s a quick teaser for y’all:

<ImgWithCaption
    alt="hackathon incoming"
    source="img/lw7/cookie-wheel.gif"
    caption="This one at least gives you a chance"
/>

## #5: See you there! 🫵

<ImgWithCaption
    alt="Not if I see you first"
    source="img/lw7/see-you-first.gif"
/>

And that’s a wrap! I hope this got you excited as much as it got me and that you will join us for the Launch Week kick-off event on Monday! You’ll get to hear about all these features and more first hand from working on them, ask questions, and learn more about what’s coming next.

Register for the event [here](https://discord.gg/K4dEEvyR?event=1291419619948626040), and make sure to mark yourself as interested 👇

<ImgWithCaption
    alt="Join the kick-off event"
    source="img/lw7/lw7-discord-event.png"
/>

## Stay in the loop

<ImgWithCaption
    alt="dont leave"
    source="img/lw7/dont-go.gif"
/>

Every day, we'll update this page with the latest announcement of the day - to stay in the loop, [follow us on Twitter/X](https://twitter.com/WaspLang) and [join our Discord](https://discord.gg/rzdnErX) - see you there!