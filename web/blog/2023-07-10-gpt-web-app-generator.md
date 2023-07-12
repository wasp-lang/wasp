---
title: 'GPT Web App Generator - Let AI create a full-stack React & Node.js codebase based on your description 🤖🤯'
authors: [martinsos]
image: /img/gpt-wasp/gpt-wasp-thumbnail.png
tags: [wasp-ai, GPT]
---

import Link from '@docusaurus/Link';
import useBaseUrl from '@docusaurus/useBaseUrl';

import ImgWithCaption from './components/ImgWithCaption'

<ImgWithCaption
    source="img/gpt-wasp/thumbnail-yellow.png"
/>

This project started out as an experiment - we were interested if, given a short description, GPT can generate a full-stack web app in React & Node.js. The results went beyond our expectations!

## How it works

All you have to do in order to use [GPT Web App Generator](https://magic-app-generator.wasp-lang.dev/) is **provide a short description of your app idea in plain English**. You can optionally select your app's brand color and the preferred authentication method (more methods coming soon).

<ImgWithCaption
    source="img/gpt-wasp/how-it-works.gif"
    caption="1. Describe your app 2. Pick the color 3. Generate your app 🚀"
/>

That's it - in a matter of minutes, a full-stack web app codebase, written in React, Node.js, Prisma, and Wasp, will be generated right in front of you, and available for you to download, run it locally and deploy with a single CLI command!

See a full one-minute demo here:
<div className='flex justify-center'>
    <iframe width="700" height="400" src="https://www.youtube.com/embed/u0MVsPb2MP8" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>
</div>

## The stack 📚

Besides React & Node.js, GPT Web App Generator uses [Prisma](https://www.prisma.io/) and [Wasp](https://github.com/wasp-lang/wasp).

[Prisma](https://www.prisma.io/) is a type-safe database ORM built on top of PostgreSQL. It makes it easy to deal with data models and database migrations.

[Wasp](https://github.com/wasp-lang/wasp) is a batteries-included, full-stack framework for React & Node.js. It takes care of everything from front-end to back-end and database along with authentication, sending emails, async jobs, deployment, and more.

Additionaly, all the code behind GPT Web App Generator is completely open-source: [web app](https://github.com/wasp-lang/wasp/tree/wasp-ai/wasp-ai), [GPT code agent](https://github.com/wasp-lang/wasp/tree/wasp-ai/waspc/src/Wasp/AI).

## What kind of apps can I build with it?
:::caution

Since this is a GPT-powered project, it's output is not 100% deterministic and small mistakes will sometimes occur in the generated code. For the typical examples of web apps (as seen below) they are usually very minor and straightforward to fix.
If you get stuck, [ping us on our Discord](https://discord.gg/rzdnErX).

:::

The generated apps are full-stack and consist of front-end, back-end and database. Here are few of the examples we successfully created:

### My Plants - track your plants' watering schedule 🌱🚰

<ImgWithCaption
    source="img/gpt-wasp/my-plants.png"
/>

- See the generated code and run it yourself [here](https://magic-app-generator.wasp-lang.dev/result/3bb5dca2-f134-4f96-89d6-0812deab6e0c)

This app does exactly what it says - makes sure that you water your plants on time! It comes with a fully functioning front-end, back-end and the database with `User` and `Plant` entities. It also features a [full-stack authentication](/blog/2023/04/12/auth-ui) (username & password) and a Tailwind-based design.

The next step would be to add more advanced features, such as email reminders (via [Wasp email sending support](/docs/guides/sending-emails)) when it is time to water your plant.

You can see and download the [entire source code](https://magic-app-generator.wasp-lang.dev/result/3bb5dca2-f134-4f96-89d6-0812deab6e0c) and add more features and deploy the app yourself!

### ToDo app - a classic ✅

<ImgWithCaption
    source="img/gpt-wasp/todo-app.png"
/>

- See the generated code and run it yourself [here](https://magic-app-generator.wasp-lang.dev/result/07ed440a-3155-4969-b3f5-2031fb1f622f)

What kind of a demo would this be if it didn't include a ToDo app? GPT Web App Generator successfully scaffolded it, along with all the basic functionality - creating and marking a task as done.

With the foundations in place (full-stack code, authentication, Tailwind CSS design) you can [see & download the code here](https://magic-app-generator.wasp-lang.dev/result/07ed440a-3155-4969-b3f5-2031fb1f622f) and try it yourself!

### Limitations

In order to reduce the complexity and therefore mistakes GPT makes, for this first version of Generator we went with the following limitations regarding generated apps:

1. No additional npm dependencies.
2. No additional files beyond Wasp Pages (React) and Operations (Node). So no additional files with React components, CSS, utility JS, images or similar.
3. No TypeScript, just Javascript.
4. No advanced Wasp features (e.g. Jobs, Auto CRUD, Websockets, Social Auth, email sending, …).

## Summary & next steps

As mentioned above, our goal was to test whether GPT can be effectively used to generate full-stack web applications with React & Node.js. While it's now obvious it can, we have lot of ideas for new features and improvements.

### Challenges
While we were expecting the main issue to be the size of context that GPT has, it turned out to be that the bigger issue is its “smarts”, which determine things like its planning capabilities, capacity to follow provided instructions (we had quite some laughs observing how it sometimes ignores our instructions), and capacity to not do silly mistakes. We saw GPT4 give better results than GPT3.5, but both still make mistakes, and GPT4 is also quite slow/expensive. Therefore we are quite excited about the further developments in the field of AI / LLMs, as they will directly affect the quality of the output for the tools like our Generator.

### Next features wishlist

1. Get feedback on this initial experiment - both on the Generator and the Wasp as a framework itself: best place to leave us feedback is on our [Discord](https://discord.com/invite/rzdnErX).
2. Further improve code agent & web app.
3. Release new version of `wasp` CLI that allows generating new Wasp project by providing short description via CLI. Our code agent will then use GPT to generate project on the disk. This is already ready and should be coming out soon.
4. Also allow Wasp users to use code agent for scaffolding specific parts of their Wasp app → you want to add a new Wasp Page (React)? Run our code agent via Wasp CLI or via Wasp vscode extension and have it generated for you, with initial logic already implemented.
5. As LLMs progress, try some alternative approaches, e.g. try fine-tuning an LLM with knowledge about Wasp, or give LLM more freedom while generating files and parts of the codebase.
6. Write a detailed blog post about how we implemented the Generator, which techniques we used, how we designed our prompts, what worked and what didn’t work, … .

## Support us! ⭐️

If you wish to express your support for what we are doing, consider giving us a [star on Github](https://github.com/wasp-lang/wasp)! Everything we do at Wasp is open source, and your support motivates us and helps us to keep making web app development easier and with less boilerplate.
