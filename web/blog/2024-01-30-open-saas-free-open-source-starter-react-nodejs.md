---
title: 'Open SaaS: our free, open-source SaaS starter'
authors: [vinny]
image: /img/open-saas/open-saas-ph.png
tags: [saas, boilerplate, react, nodejs]
---

import ImgWithCaption from './components/ImgWithCaption'

## Presenting Open SaaS 🎉 

We’re really excited to present [Open SaaS](https://opensaas.sh), the totally free, open-source, production-grade SaaS boilerplate for React, NodeJS, and Prisma.

Check out the promo video here:

<iframe width="560" height="315" src="https://www.youtube.com/embed/rfO5SbLfyFE?si=IqTfhVk7DV5VsiaL" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>

Open SaaS has got all the features of those paid SaaS starters you’ve been seeing lately, except its entirely **free** and **open-source**. 

We felt that paying $300-$2,000 for some boilerplate code that you need to manage yourself was crazy. On top of that, many of these boilerplates rely heavily on 3rd-party services. Add on hosting and other fees, and you’re looking at spending quite a bit of money just to get your idea out there into the world.

That’s why with Open SaaS we made a conscious decision to try and use open-source and free services whenever possible. For example, our hosted demo app and its admin dashboard on [OpenSaaS.sh](http://OpenSaaS.sh) are powered by a self-hosted version of Plausible analytics. Want the same features in your SaaS? Well, Open SaaS has got it preconfigured for you!

Also, the [Wasp framework](https://wasp.sh), which Open SaaS uses, does the job of building out a number of features for you, like Auth and Cron Jobs, so that you don’t have to pay a 3rd-party service or code it entirely yourself (we’ll explain this in more detail later).

## Before we start...

<a href="https://www.producthunt.com/posts/open-saas?utm_source=badge-featured&utm_medium=badge&utm_souce=badge-open&#0045;saas" target="_blank"><img src="https://api.producthunt.com/widgets/embed-image/v1/featured.svg?post_id=436467&theme=light" alt="Open&#0032;SaaS - Open&#0045;source&#0032;&#0038;&#0032;100&#0037;&#0032;free&#0032;React&#0032;&#0038;&#0032;Node&#0046;js&#0032;SaaS&#0032;starter&#0033; | Product Hunt" /></a>

Open SaaS is [live on Product Hunt](https://www.producthunt.com/posts/open-saas) right now! Come support our free, open-source initiative 🙏

[![Image description](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/wppn8mlby0p7h1f8xl6w.png)](https://www.producthunt.com/posts/open-saas)


## Why we built it… and then gave it away for free

The initial feedback in [our pre-release](https://devhunt.org/tool/open-saas) has been largely positive, but we’ve also gotten some questions like:
- “Is it going to stay free?”
- “What’s your motivation for open-sourcing this?”

So we thought we’d go ahead and answer these to start.

![Image description](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/5rac9o1rxgrwfx51mc50.png)

First, yes it is 100% free and open-source and will stay that way.

Second, we believe that the collective knowledge of a community of developers, indiehackers, and solopreneurs will produce a better boilerplate than an individual or small group. When you buy a SaaS starter from some developer, you’re already getting an opinionated stack, then on top of that you’re also getting an app built the way they think is best — and that may not always be the best *for you.* 

Third, [Open SaaS](https://opensaas.sh) is a project by [Wasp](https://wasp.sh), an open-source React + NodeJS + Prisma full-stack framework with superpowers. We, the Wasp team, believe that Wasp is very well suited for creating SaaS apps quickly and efficiently, and we want this template to prove it. Plus, as developers, we’ve learned so much from other open-source projects, and Wasp itself is an open-source project. 

Basically, we love the open-source philosophy and we want to pay it forward. 🙏

So it’s our hope that we can provide a seriously valuable asset to the developer community while spreading the word about our open-source, full-stack framework. And we’d love to see the community contribute to it so that it will grow and become the best SaaS boilerplate out there.

## What Open SaaS is Made Of

We put a lot of hard work into Open SaaS, including the [documentation](https://docs.opensaas.sh), so that developers can get a SaaS app launched confidently and easily.

We’ve also spent some time checking out other free, open-source SaaS starters, and wanted to make sure Open SaaS has all the right features of a production-ready starter, without the bloat. And we think we’ve accomplished that for the most part, although we will continue to add features and improve on it with time. 

Here are the main features at the moment:

- 🔐 Authentication (email verified, google, github)
- 📩 Emailing (sendgrid, emailgun, SMTP)
- 📈 Admin Dashboard (plausible or google analytics)
- 🤑 Stripe payments (just add your subscription product IDs)
- ⌨️ End-to-end Typesafety (no configuration necessary)
- 🤖 OpenAI integrated (AI-powered example apps)
- 📖 Blog w/ Astro
- 🚀 Deploy anywhere
- 📄 Full Documentation & Community Support

It’s worth going into some detail about each of these features, so let’s do it.

### Auth


[![Image description](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/wbistoghxrxft9zxxra1.png)](https://www.producthunt.com/posts/open-saas)

Thanks to Wasp, Open SaaS ships with a number of possible Auth methods:

- username and password (simplest/easiest for dev testing)
- email verified w/ password reset
- Google and/or Github social login

Here’s where Wasp really shines, because all it takes to set up your full-stack Auth and get pre-configured UI components is this:

```jsx
//main.wasp
app SaaSTemplate {
  auth: {
    userEntity: User,
    methods: {
      usernameAndPassword: {},
      google: {},
      gitHub: {},
    }
  }
}
```

Seriously. That’s it!

Just make sure you’ve set up your social auth and have your API keys, as well as your `User` and `ExternalAuth` entities defined, and you’re good to go. And don’t worry, that part is all documented and explained in detail in the [Open SaaS Docs](https://docs.opensaas.sh).

On top of that, Open SaaS comes preconfigured with some examples on how to customize and create some really powerful auth flows.

### Admin Dashboard & Analytics

[![Image description](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/4mm6s1c3txxgm49e2k7w.png)](https://www.producthunt.com/posts/open-saas)

By leveraging [Wasp’s Jobs feature](https://wasp-lang.dev/docs/advanced/jobs), Open SaaS pulls data from Plausible’s or Google’s Site Analytics (your choice!) and Stripe’s Data APIs every hour and saves them to our database. This data is then shown on our Admin Dashboard (go to [OpenSaaS.sh](https://OpenSaaS.sh) to see it in action). The nice part is, to get access to this data for your own app, all you have to do is follow our guide on getting your analytics API keys, insert the provided script, and you’re good to go!

Again, Wasp makes this whole process really easy. With the function for querying the APIs and getting the data we need already defined for you, Open SaaS then uses a Wasp Job within the `main.wasp` config file:

```jsx
job dailyStatsJob {
  executor: PgBoss,
  perform: {
    fn: import { calculateDailyStats } from "@server/workers/calculateDailyStats.js"
  },
  schedule: {
    cron: "0 * * * *" 
  },
  entities: [User, DailyStats, Logs, PageViewSource]
}
```

And that’s it! Wasp takes care of setting up and running the cron job for you.

### Stripe Payments

[![Image description](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/ugy3mx9xo1d9i9vfysr7.png)](https://www.producthunt.com/posts/open-saas)

If you’re a developer that’s never built your own SaaS before, then integrating with a payments processor like Stripe is probably one of the few challenges you’ll face.

This was the case for me when I built my first SaaS, [CoverLetterGPT.xyz](https://coverlettergpt.xyz). That was actually one of my main motivators for building it; to learn how to intergrate Stripe payments into an app, as well as the OpenAI API.

And even though Stripe is well known for having great documentation, the process can still be daunting. You have to:

- create the correct product type
- set up webhook endpoints
- tell Stripe to send the correct webhook events to you
- consume the events correctly
- deal with recurring and failed payments
- test it all correctly via the CLI before going live

That’s why having Stripe subscription payments set up for you is such a win. 

But even more important than that, is having the whole process conveniently documented for you! Which is why Open SaaS offers you convenient [Stripe guides in our documentation](https://docs.opensaas.sh) 🙂

[![Image description](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/uehwot350u3dl02s4w7r.png)](https://www.producthunt.com/posts/open-saas)

### End-to-End Typesafety

Open SaaS was built with Typescript, and because it’s a full-stack app, type safety from the back-end to the front-end can be a real lifesaver. I mean, some [opinionated stacks](https://create.t3.gg/) have gotten hugely popular on this basis.

Luckily, Wasp gives you end-to-end Typesafety out-of-the-box (nothing to configure!), so it was easy for Open SaaS to take advantage of it. 

Here’s an example:

1. Make Wasp aware of your server action:
    
    ```tsx
    // main.wasp
    
    action getResponse {
      fn: import { getResponse } from "@server/actions.js",
      entities: [Response]
    }
    ```
    
2. Type and Implement your server action.
    
    ```tsx
    // src/srever/actions.ts
    
    type RespArgs = {
      hours: string;
    };
    
    const getResponse: GetResponse<RespArgs, string> = async ({ hours }) => { }
    ```
    
3. Import it and call it on the client.
![Image description](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/0fah81r1g4bg3vdqapju.png)
Client-side types will be inferred correctly!
![Image description](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/7n04yh6de9slhhnjrgf3.png)

### AI-powered Example App (w/ OpenAI API)

[![Image description](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/zbbc2gkxbxjl3q2y01a3.png)](https://www.producthunt.com/posts/open-saas)

AI is making new app ideas possible, which is partly why we’re seeing a resurgence in developer interest in creating SaaS apps. As I mentioned above, the first SaaS app I built, [CoverLetterGPT](https://coverlettergpt.xyz), is one of those “GPT Wrappers”, and I’m proud to say it makes a nice passive income of ~$350 MRR (monthly recurring revenue).

I personally believe we’re in a sweet spot in software development where there exists a lot of potential to develop new, profitable AI-powered apps, especially by "indiehackers" and "solopreneurs".

This is why Open SaaS features an AI scheduling assistant demo app. You input your tasks for along with their alotted time, and the AI Scheduler creates a detailed plan for your day.

[![Image description](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/j4suf7g9jm5w93ri3bqx.png)](https://www.producthunt.com/posts/open-saas)

Under the hood, this is using OpenAI’s API to assign each task a priority, and break them up into detailed sub-tasks, including coffee breaks! It’s also leverages OpenAI’s function calling feature to return the response back in a user-defined JSON object, so that the client can consume it correctly every time. Also, we're planning on adding open-source LLMs in the future, so stay tuned!

The demo AI Scheduler is there to help developers learn how to use the OpenAI API effectively, and to spark some creative SaaS app ideas!

### Deploy Anywhere. Easily.

A lot of the popular SaaS starters out there use hosting-dependent frameworks, which means you're stuck relying on one provider for deployments. While these can be easy options, it may not always be the best for your app.

Wasp gives you endless possibilities for deploying your full-stack app:

- One-command deploy to [Fly.io](http://Fly.io) with `wasp deploy`
- Use `wasp build` and deploy the Dockerfiles and client wherever you like!

The great thing about `wasp deploy`, is that it automatically generates and deploys your database, server, and client, as well as sets up your environment variables for you.

Open SaaS also has built in environment variable and constants validators to make sure that you’ve got everything correctly set up for deployment, as well as deployment guides in the docs

[![Image description](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/fihbij250xtbdtjbjoks.png)](https://www.producthunt.com/posts/open-saas)

In the end, you own your code and are free to deploy it wherever, without vendor lock-in.

## Help us, help you

<a href="https://www.producthunt.com/posts/open-saas?utm_source=badge-featured&utm_medium=badge&utm_souce=badge-open&#0045;saas" target="_blank"><img src="https://api.producthunt.com/widgets/embed-image/v1/featured.svg?post_id=436467&theme=light" alt="Open&#0032;SaaS - Open&#0045;source&#0032;&#0038;&#0032;100&#0037;&#0032;free&#0032;React&#0032;&#0038;&#0032;Node&#0046;js&#0032;SaaS&#0032;starter&#0033; | Product Hunt"/></a>

Wanna support our free, open-source initiative? Then go show us some support [on Product Hunt](https://www.producthunt.com/posts/open-saas) right now! 🙏

[![Image description](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/wppn8mlby0p7h1f8xl6w.png)](https://www.producthunt.com/posts/open-saas)

## Now Go Build your SaaS!

We hope that Open SaaS empowers more developers to ship their ideas and side-projects. And we also hope to get some feedback and input from developers so we can make this the best SaaS boilerplate starter out there. 

So, please, if you have any comments or catch any bugs, submit [an issue here](https://github.com/wasp-lang/open-saas/issues).

And if you’re finding Open SaaS and/or Wasp useful, the easiest way to support is by throwing us a star:

- Star the [Open SaaS repo](https://github.com/wasp-lang/open-saas)

