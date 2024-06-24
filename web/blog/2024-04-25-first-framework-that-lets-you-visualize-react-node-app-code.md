---
title: "The first framework that lets you visualize your React/NodeJS app's code"
authors: [vinny]
image: /img/wasp-studio/baby-sleep-tracker-studio.png
tags: [react, nodejs]
---

import ImgWithCaption from './components/ImgWithCaption'

<ImgWithCaption
    alt="Wasp Studio screenshot, baby sleep tracker"
    source="img/wasp-studio/baby-sleep-tracker-studio.png"
/>

## Visualize the Prize

Imagine you’re working on your full-stack app, and you want to implement a new feature. It’s a complicated one, so you whip out a pen and paper, or head over to [tldraw](https://www.tldraw.com/), and start drawing a diagram of what your app currently looks like, from database, to server, and on over to the client.

But how cool would it be if you had a **tool that visualized your entire full-stack app for you**? And what if that tool had the potential to do greater things, like instantly add useful functionality for you across your entire stack, or be paired with AI and Large Language Models for code generation?

Well, that idea is already a reality, and it’s called `wasp studio`. Check it out here:

<div className='video-container'>
    <iframe src="https://www.youtube.com/embed/SIAhAvDEoMw?si=5hOEvsQ2P6uCTUPn" frameborder="1" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>
</div>

## Wasp Studio is the Name

First off, [Wasp](https://github.com/wasp-lang/wasp) is a full-stack React, NodeJS, and Prisma framework with superpowers. It just crossed [10,000 stars on GitHub](https://github.com/wasp-lang/wasp), and it has been used to create over 50,000 projects.

Why is it special? It uses a config file and its own compiler to manage a bunch of features for you, like auth, cron jobs, routes, and email sending, saving you tons of time and letting you focus on the fun stuff.

<ImgWithCaption
    alt="Wasp how-it-works diagram"
    source="img/wasp-studio/wasp-diagram.png"
/>

This combination of [Wasp’s central config file](/docs), which acts as a set of instructions for the app, and compiler also allow Wasp to do a bunch of complex and interesting tasks for you via one-line commands, such as:

- full-stack deployments → `wasp deploy`
- starting a development database with Docker → `wasp start db`
- scaffold entire example apps, such as a SaaS starter → `wasp new`
- giving you a visual schematic of your entire full-stack app → `wasp studio`

If you wanna try them out yourself all you have to do is:

1. [install Wasp](/docs/quick-start) with `curl -sSL https://get.wasp-lang.dev/installer.sh | sh`
2. scaffold a new To Do app in TypeScript with `wasp new -t todo-ts`
3. then to get the visualizer as in the screenshot below, run `wasp studio`

<ImgWithCaption
    alt="Baby Sleep Tracker in Wasp Studio"
    source="img/wasp-studio/baby-tracker-full.png"
/>

Let’s break down what we’re seeing here real quick:

- Our main App component in the middle in blue shows the app’s name, database we’re using, and its Auth method
- Entities to the left in yellow show us which database models we’ve defined
- Actions and Queries to the far left in red and green show us our server operations that act on our database entities
- Routes and Pages on the right show us where our React components live and if they require authorization or not (denoted by 🔒)

And if you’re wondering what this might look like with a more complex app, here’s what it looks like when run against [Open SaaS - our free, open-source SaaS boilerplate starter](https://github.com/wasp-lang/open-saas).

<ImgWithCaption
    alt="Open SaaS in Wasp Studio"
    source="img/wasp-studio/opensaas-studio.png"
/>

What’s great about this is that we have an overview of all our database entities and which server functions (aka “operations”) they depend on.  In the top left of the picture above, you’ll even see a cron job, `dailyStatsJob`, which runs every hour (`0 * * * *`). 

This, for example, makes developing backend logic a breeze, especially if you’re not a seasoned backend developer. Consider that the code that gets you there is as simple as this:

```c
job dailyStatsJob {
  executor: PgBoss,
  perform: {
    fn: import { calculateDailyStats } from "@src/calculateDailyStats"
  },
  schedule: {
    cron: "0 * * * *" 
  },
  entities: [User, DailyStats, Logs, PageViewSource]
}
```

Yep, that’s all it takes for you to get asynchronous jobs on your server. Now your `calculateDailyStats` function will run every hour — no third party services needed 🙂

## Is this a Party Trick!?

Ok. You might be thinking, the visualizer is cool, but does it actually serve a purpose or is it just a nice “party trick”? And to be honest, *for now* it is a party trick.

But it’s a party trick with **a lot of potential** up its sleeve. Let me explain.

<ImgWithCaption
    alt="You got potential, kid."
    source="img/wasp-studio/potential.gif"
/>

Of course, you can use it in its current form to get a better perspective of your app, or maybe plan some new features, but in the future you will be able to use it to do a lot more, such as:

- add new auth methods with a few clicks
- quickly scaffold functional client-side components with server operations
- instantly add new full-stack functionality to your entire app, like Stripe payments
- collaborate easily with Large Language Models (LLMs) to generate features on-the-fly!

Again, this is all possible because of the central configuration file which acts as a set of “instructions” for your app. With this file Wasp literally knows how your app is built, so it can easily display your app to you in visual form. It also makes it a lot easier to build new parts of your app for you in exciting new ways.

Take a look at another snippet from a Wasp config file below. This is all it takes to get full-stack auth for your web app! That’s because the Wasp compiler is managing that boilerplate code for you.

```c
app todoVisualize {
  title: "todo-visualize",

  auth: {
    userEntity: User,
    methods: {
      usernameAndPassword: {}, 
      google: {},
    },
  }
}

entity User {=psl
  id          Int     @id @default(autoincrement())
  tasks       Task[]
psl=}
```
# A Picture is Worth a Thousand Tokens

Now that we know a bit about how Wasp works, let’s dive deeper into the potential of Wasp and `wasp studio` in combination with LLMs as a future use case. 

Currently one of the biggest constraints to AI-assisted code generation is context. By now, we all know that LLMs like to hallucinate, [but they also have a pretty bad “memory”](https://glazkov.com/2023/05/18/ai-developer-experience-asymptotes/). So, if you were to try and get them to build features for your app, to make sure that the new feature works with it, you have to constantly “remind” them of how the app works, its structure, and dependencies.

But with Wasp’s config file, which is essentially just a higher-level abstraction of a full-stack app and its features, we give the LLM the context it needs to successfully build new features for the app at hand.

<ImgWithCaption
    alt="PG tweet"
    source="img/wasp-studio/pg-tweet.png"
/>

And this works really well because we don’t only give the LLM the context it needs, but Wasp’s compiler also takes on the responsibility of writing most of the boilerplate for us to begin with (thanks, pal), giving the LLM the simpler tasks of writing, e.g.:

- modifications to the Wasp config file
- functions to be run on the server
- React components that use Wasp code

In this sense, the LLM has to hold a lot less in context and can be forgiven for its bad memory, because Wasp is the one making sure everything stays nicely glued together! 

To further bring the point home,  let’s take a look again at that Auth code that we introduced above:

```c
auth: {
  userEntity: User,
  methods: {
    usernameAndPassword: {}, 
    google: {},
  },
```

Consider that this code gives auth across your entire stack. So, not only do you get all the auth logic generated and managed for you on the server, but [you even get UI components and auth hooks](/docs/auth/ui) made available to you on the client! 

<ImgWithCaption
    alt="Wasp Auth UI"
    source="img/wasp-studio/wasp-auth-ui.gif"
/>

On the other hand, without the abstractions that Wasp gives us, we end up relying on the LLM, with it’s bad memory and tendency to hallucinate, to write a bunch of boilerplate for us over and over again like this JWT middleware pictured below:

<ImgWithCaption
    alt="Auth without Wasp"
    source="img/wasp-studio/auth-node.png"
/>

And LLMs are pretty good at coding boilerplatey, repetitive tasks in isolation. But expecting them to do it as part of a cohesive full-stack app means that we have a ton more surface area for exposure to possible errors. 

With Wasp, on the other hand, **it’s just a few lines of code**. If it’s easy for humans to write, it’s also super easy for an LLM to write. 

By the way, not only does this save us a lot of headache, it also can save us a lot of money too, as [AI-generated Wasp apps use ~10-40x less tokens (i.e. input and output text) than comparable tools](/blog/2023/07/17/how-we-built-gpt-web-app-generator), so they generate code at a fraction of the price.

## Helping the Computers Help Us

As technologies continue to improve, programming will become more accessible to users with less expert knowledge because more of that expert knowledge will be embedded in our tools.

But that means we will need abstractions that allow for us, the humans, to work easily with these tools. 

Like the LLM example above, we can build tools that get AIs to write all the boilerplate for us over and over again, but the question is, *should* we be letting them do that when they *could* be doing other more useful things? LLMs are great at producing a wealth of new ideas quickly. Why not build tools that let AIs help us in this regard?

That’s exactly what we have planned for the future of `wasp studio`. A visual interface that allows you to piece together new features of your app, with or without the help of LLMs, and then A/B test those different ideas quickly.

<div className='video-container'>
    <iframe src="https://www.youtube.com/embed/ERwtJtNQL28?si=1QtV3DnfQCHjRQrK" frameborder="1" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>
</div>

Not only that, but we also then have an abstraction at our disposal that allows for easy collaboration with users who are less technically inclined. With the help of such tools, even your Product Manager could get in on the fun and start building new features for the developers to sign off on.

What’s so powerful about Wasp and its feature set, is that we get code that’s **simpler to read, debug, and maintain,** for both people and machines. Coupled with a visual interface, we will be able to quickly iterate on new features across the entire stack, using it as a planning and orchestration tool ourselves, or as a way to more easily debug and oversee the work an LLM might be doing for us.

This is a pretty exciting look at the future of web development and with these new tools will come lots of new ways to utilize them. 

What are some ways that you think a tool like `wasp studio` could be used? What other developments in the realm of AI x Human collaboration can you imagine are coming soon?
