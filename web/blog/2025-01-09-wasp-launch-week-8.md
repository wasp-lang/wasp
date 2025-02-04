---
title: "Wasp Launch Week #8 - it's a Fixer Upper 🛠️🏡"
authors: [matijasos]
image: /img/lw8/lw8-banner.png
tags: [launch-week, update]
---

import Link from '@docusaurus/Link';
import useBaseUrl from '@docusaurus/useBaseUrl';

import InBlogCta from './components/InBlogCta';
import WaspIntro from './_wasp-intro.md';
import ImgWithCaption from './components/ImgWithCaption'

<ImgWithCaption
    alt="Launch Week 8 is here"
    source="img/lw8/lw8-banner.png"
/>


Hola Waspeteers,

we're wishing you a happy New Year and hope you had nice and relaxing holidays (and managed to do some coding in between all the food, of course)! In case you haven't heard yet, this year is the year of Wasp 🐝! So we'll immediately kick it off with a bunch of cool new features we've been cooking for the last few months:

<ImgWithCaption
    alt="Buckle up"
    source="img/lw8/buckle-up.webp"
    caption="Yup, you better do."
/>

As the olden tradition goeth, started by Wasp druids almost one thousand days ago (that's how you make two years sound longer), we'll do a big reveal during our community call that will take place next Monday - January 13th, 10.30 AM EST / 4.30 PM CET! To reserve your spot, [visit the event in our Discord server](https://discord.gg/PQVY9FvG?event=1326930588191559690) and mark yourself as interested.

<ImgWithCaption
    alt="Join the kick-off event"
    source="img/lw8/join-event-instructions.png"
/>

Be there or be square! Do it now, so we know how many virtual pizzas and drinks we have to order.

## Why a Fixer Upper?

In the last seven Launch Weeks we've introduced a lot of new features and concepts - some are things you'd expect a mature web framework to have, and others are things you haven't [ever seen before](/blog/2024/04/25/first-framework-that-lets-you-visualize-react-node-app-code). This has proven to be a winning combination in pushing Wasp forward - it's both about reaching 1.0, but also showing why Wasp's compiler-driven approach is so useful and powerful.

Having been doing this for a while, almost two years since our first launch week, Wasp has grown a lot and come to that point where you are building and deploying (some even selling) their applications. We've realized we've come to the stage where things work, and they actually work pretty well.

<ImgWithCaption
    alt="I love Wasp"
    source="img/lw8/i-love-wasp.png"
/>

Having also received a ton of feedback from you on different topics, we decided to dedicate a full quarter of building exclusively on that! It's not only about building features, it's also about all the other "little" things which make for an amazing developer experience we're aiming for - documentation, examples, guides & tutorials, integrations with other tools. We went full in on it, and hopefully it will make your life a little bit easier next time you decide to use Wasp to build your app.

Now, let's take a look together at all the nice things which this latest release of Wasp brings us:

## #1: Deployment Games 🦑: Docs revamp + easily self-host with Coolify, CapRover, and more!

<ImgWithCaption
    alt="Deployment games"
    source="img/lw8/deployment-games.png"
    caption="Take the blue pill, Harry."
/>

We all know what choosing a hosting and deployment method can feel like these days - a hundred and one way to go about it, but make a wrong choice and you're in for a trouble.

This is why more and more developers are looking to learn more about deployment options and use a stack that's flexible enough to be deployed pretty much anywhere.

It's also how we think about it at Wasp - we want to make it as easy as possible for you to use a service you know and like (e.g. Fly, Railway, or Supabase), but also go the self-hosting route if that's what you prefer (e.g. Coolify or CapRover).

<ImgWithCaption
    alt="deployment diagram"
    source="img/lw8/wasp-app-flow.gif"
    caption="Miho add some super cool diagrams like this in our revamped docs!"
/>

For this launch week, we did a complete revamp of our Deployment docs section - it's much more detailed and contains answers to the common questions we've been getting from the community. Additionaly, we've included step-by-step guides on how to self host your Wasp app with Coolify and CapRover.

Bon Deploittit! (sorry about this)

## #2: Validation bonanza 🏴‍☠️: Validate your env vars with Zod!

<ImgWithCaption
    alt="env var validation with Zod"
    source="img/lw8/zod-env-validation.png"
/>

Environment variables are probably one of the most common causes of making you pull out hair when trying to finally put your app in production. It's one of the facts of life (right up there with paying taxes), so not trying to say we're gonna fix it, but we can make it at least a bit easier (more time for taxes).

From now, **you can use Zod to validate your env variables, both those that are required by Wasp, and your custom ones**! Determine type, error message and everything else that comes to your mind, so you actually feel (almost) happy when you realize there is a missing env var in your app.

## #3: Community day: Starter templates, apps made (and sold) with Wasp, and more!

<ImgWithCaption
    alt="TurboReel"
    source="img/lw8/turboreel.webp"
    caption="Made with Wasp!"
/>

Thanks to you, our dear Waspeteer and community member, we have a lot to write about here, so we decided to dedicate a whole day to you! There have been so many cool apps, milestones and contributions to Wasp that we simply have to show it all off.

Get ready for the day of showcasing apps made with Wasp, success stories, integrations with AI development tools, community-made starter templates and more!

## #4: TypeScript all the way: Wasp now respects your own ts.config file!

<ImgWithCaption
    alt="tsconfig"
    source="img/lw8/tsconfig-wasp.png"
    caption="You will respect my authoritah!"
/>

Wasp is doing a lot of things for you - managing your front-end, back-end, database, deployment and pretty much everything in between. It means it is aware of a lot of things in your codebase, but sometimes it can get a bit too much and make you feel like you're out of control.

To reduce the sentient-AI vibes ("I'm sorry Dave, I'm afraid I can't do that" - anyone?) we started with making sure that Wasp takes into account whatever you defined in your tsconfig.json and follows it (take that, AI). That means you have an easier way defining your import aliases (e.g. required by Shadcn), better error messages with proper file paths, ....

All in all, a bit of respect goes a long way!

## #5: OpenSaaS day: cursorrules, YouTube walkthrough video and Vinny getting back to what he does best 🏗️

<ImgWithCaption
    alt="opensaas video tutorial"
    source="img/lw8/opensaas-video.png"
    caption="Ma, cancel Netflix, Vinny just dropped a new Open SaaS tutorial!"
/>

And finally, we'll close it with your favorite SaaS starter - OpenSaaS! Vinny and the team have been casting their magic with no rest, and along with the improved template, also created a full-fledged video tutorial on how to start your SaaS with it! Follow it for step-by-step instructions and enjoy Vinny's smooth espresso-coated voice.

## Stay in the loop

<ImgWithCaption
    alt="dont leave"
    source="img/lw7/dont-go.gif"
/>

Every day, we'll update this page with the latest announcement of the day - to stay in the loop, [follow us on Twitter/X](https://twitter.com/WaspLang) and [join our Discord](https://discord.gg/rzdnErX) - see you there!
