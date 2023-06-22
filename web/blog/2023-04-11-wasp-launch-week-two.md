---
title: 'Wasp Launch Week #2'
authors: [matijasos]
image: /img/lw2/lw2-banner.png
tags: [webdev, wasp, startups, github]
---

import Link from '@docusaurus/Link';
import useBaseUrl from '@docusaurus/useBaseUrl';

import InBlogCta from './components/InBlogCta';
import WaspIntro from './_wasp-intro.md';
import ImgWithCaption from './components/ImgWithCaption'

Here we go again! After three months of building and talking to our community about what features they'd like to see next, we're proud to kick off our second Launch Week. It stars tomorrow, and you can [sign up for the launch event here](https://discord.gg/PX6KczVz?event=1093188663912964136)!

<ImgWithCaption
    alt="Launch Week 2 is coming"
    source="img/lw2/lw2-banner.png"
/>

Wasp Beta introduced a lot of core features that enabled developers to a build full-fledged SaaS-es. Since then, our community grew rapidly and we watched you deploy numerous apps and some of you even [making their startups and earning their first revenue on top of Wasp](/blog/2022/11/26/erlis-amicus-usecase)!

Seeing that all the essential building blocks are now in place, **our next goal became to make Wasp really easy (and fun) to use**. We've had a bunch of ideas on everything we'd like to improve with DX for a while, and now finally came the right time to do it.

Nonetheless, the theme and sentiment of this launch week is best captured by an ancient term that poets used to describe some of the most beautiful and marvelous wonders of the world (e.g. pyramids, or the hanging gardens of Babylon): **pizzazz** 🍕.

<!--truncate-->

## Wednesday, Apr 12 - Launch event 🚀 + Pizzazz opener: Auth UI 💅

Wasp's easy auth has been by a long shot one of the most popular features in the community. We decided to take it one step further - **Wasp now offers beautifully designed, pre-made auth components** that you can simply plug into your app and immediately get that razzle dazzle on!

<ImgWithCaption
    caption="On your localhost, tomorrow"
    alt="Auth UI Demo"
    source="img/lw2/auth-ui-demo.gif"
/>

We'll present this and much more at our Kick-off event, starting tomorrow **on our Discord at 10 am EDT / 4 pm CET** - sign up [here](https://discord.gg/PX6KczVz?event=1093188663912964136) and make sure to mark yourself as interested!

Join us to meet the team and to be the first to get a sneak peek into the latest features! We'll follow up with a casual AMA session, showcase selected community projects and discuss all together about what we'd like to see in Wasp next.

<ImgWithCaption
    alt="LW2 launch party instructions"
    source="img/lw2/discord-event-info.png"
/>

P.S. : The word is out that there will be a raffle and that the most lucky one(s) will win some cool Wasp swag! (Da Boi included, ofc).

## Thursday, Apr 13 - Deploy your app to Fly.io with a single CLI command

<ImgWithCaption
    alt="Deploying to Fly.io"
    source="img/lw2/wasp-deploy-fly.jpeg"
/>

When developing your app is blazingly fast, the last thing you want to slow you down is deployment. Figuring out how to exactly setup client/server, dealing with CORS, configuring ports and env vars, ... - well, now you don't have to think about it anymore!

This release of Wasp introduces first CLI deployment helper, for Fly.io (others coming soon, and you're free to contribute)!

<ImgWithCaption
    caption="Deployment in Wasp before vs now"
    alt="How deployment feels now"
    source="img/lw2/deployment-before-now.gif"
/>

## Friday, Apr 14 - Improved database tooling & DX

<ImgWithCaption
    alt="Database seeding"
    source="img/lw2/wasp-db-seed.png"
/>

Introducing two main quality-of-life features here:
- **`wasp start db` - Fully managed development database** - (don't ever run `docker run postgres ...` again)
- **Database seeding** - populate your database with some initial, "seed" data

This was something we ourselves ended up needing often when developing a new app, and although not a huge thing at the first glance, it's feels so good to have it taken care of! Given that Wasp is a fully managed full-stack framework that "understands" all parts of your dev process, we were in unique position to offer this functionality.

P.S. - you haven't been connecting to the prod database all along during development, have you?

## Saturday, Apr 15 - More launch goodness: Custom API routes + Email sending ✉️

It's Saturday, so you get two features for the price of one!

### Add custom API routes

<ImgWithCaption
    alt="Custom API routes"
    source="img/lw2/custom-api-route.png"
    caption="Adding a custom route handler at /foo/bar endpoint"
/>

Although for typical CRUD you don't have to define an API since Wasp offers a typesafe RPC layer via [operations](/docs/tutorials/todo-app/03-listing-tasks#introducing-operations-queries-and-actions), sometimes you need extra flexibility (e.g. for implementing webhooks). Now you can easily do it, in a typical boilerplate-free Wasp style!

### Email sending: Wasp + Sendgrid/Mailgun/...

<ImgWithCaption
    alt="Laurence Fishburne messenger pigeons"
    source="img/lw2/laurence-fishburne-pigeons.png"
    caption="Don't end up like this, use Wasp for sending emails"
/>

Email sending - another feature that sounds like you should be able to implement it in 30 minutes (looking at you, auth), but then you find yourselves a week later cursing web development and having an inexplicable urge to start breeding messenger pigeons (that's what happened to Laurence Fishburne in John Wick, if you ever wondered). 

<ImgWithCaption
    alt="Email sending code example"
    source="img/lw2/email-sending-code.png"
/>

Wasp offers unified interface for different providers (e.g. Sendgrid or Mailgun, or a custom SMTP server). It also works great with our latest auth method, `email` - you get email verification and password reset out of the box!

## Sunday, Apr 16 - Frontend testing and full-stack type safety!

We continue with our buy-one-get-one-free scheme (although both are free in all fairness):

### Frontend testing, powered by Vitest

<ImgWithCaption
    alt="Frontend testing via Vitest"
    source="img/lw2/vitest.png"
/>

All you have to do to run your frontend tests is run `wasp test client` in your CLI! Backed by Vitest, while mocking is powered by [MSW](https://mswjs.io/) and additional Wasp helpers sprinkled on top. Now you really have no excuses to write your tests (except on the backend, support for them is coming next, so enjoy while it lasts)!

### Full-stack type safety

<ImgWithCaption
    caption="Our typesafe RPC is now doing some serious type-fu"
    alt="Our RPC is now doing serious type-fu"
    source="img/lw2/type-fu.gif"
/>

We already introduced glimpses of this in our Beta launch, but now things got even better! Whatever types you define and use on the server, be it entities or your custom types, they immediately get propagated to the client and typecheck in your IDE.

## Monday, Apr 17 - SaaS GPT template + Waspathon #2 kick-off!

<ImgWithCaption
    alt="SaaS GPT template"
    source="img/lw2/wasp-saas-template.png"
/>

Aaand we saved the best for the last - we'll put a special highlight on our [SaaS GPT starter](https://github.com/wasp-lang/starters#saas-template), which lets you **build GPT-powered apps (such as [CoverLetterGPT.xyz](https://coverlettergpt.xyz/) or [SocialPostGPT.xyz](https://socialpostgpt.xyz/)) in a day and with all the good stuff pre-included - auth (social, email), Tailwind, deployment, Stripe and GPT API integration**, ... - all you need to do is run it and start coding!

### Our second hackathon - Waspathon #2!

<ImgWithCaption
    caption="Hate it when this happens."
    alt="Hacking away"
    source="img/lw2/hackathon.gif"
/>

**And what a better reason to try out the SaaS GPT template than a hackathon**! It will be an open format and you're free to build whatever you want - there will be a few categories will grade and award, but more on that coming soon!

The same for the prizes - expect cool wasp-themed swag and useful stuff that makes dev's life easier (no, it doesn't include getting rid of your PM).

We'll share more info and the registration link soon.

## Recap

- **We are kicking off Launch Week #2 on Wed, April 12, at 10am EDT / 4pm CET** - make sure to [register for the event](https://discord.gg/PX6KczVz?event=1093188663912964136)!
- **Launch Week #2 brings a ton of new exciting features** - we’ll highlight one each day, starting tomorrow
- **On Monday, April 17, we’ll announce a hackathon** - follow us on [twitter](https://twitter.com/WaspLang) and [join our Discord](https://discord.gg/rzdnErX) to stay in the loop!

That’s it, Waspeteers - put your pizzazz (buzzazz?) on and see you tomorrow! 🐝

Matija, Martin & the Wasp team
