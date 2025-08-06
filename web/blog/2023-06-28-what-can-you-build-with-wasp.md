---
title: 'What can you build with Wasp?'
authors: [matijasos]
image: /img/build-with-wasp/build-with-wasp-banner.png
tags: [launch-week, showcase]
---

import Link from '@docusaurus/Link';
import useBaseUrl from '@docusaurus/useBaseUrl';

import InBlogCta from './components/InBlogCta';
import WaspIntro from './_wasp-intro.md';
import ImgWithCaption from './components/ImgWithCaption'

<ImgWithCaption alt="Launch Week 3 is coming" source="img/build-with-wasp/build-with-wasp-banner.png" />

Welcome to the 3rd day of our [Launch Week #3](/blog/2023/06/22/wasp-launch-week-three) - Community Day! Our community is the most important aspect of everything we do at Wasp, and we believe it's only right to have a day dedicated to it.

We'll showcase some of the coolest project built with Wasp so far and through that explore together what kind of apps you can develop with it. Let's dive in!

:::tip

If you're looking for a quick way to start your project, check out our [Ultimate SaaS Starter](https://github.com/wasp-lang/SaaS-Template-GPT). It packs Tailwind, GPT, Stripe ane other popular integrations, all pre-configured for you.

:::

## [CoverLetterGPT.xyz](https://coverlettergpt.xyz/) - GPT-powered cover letter generator

<ImgWithCaption source="img/build-with-wasp/cover-letter-gpt.png" />

**Try it out**: [coverlettergpt.xyz](https://coverlettergpt.xyz/)

**Source code**: https://github.com/vincanger/coverlettergpt

**Wasp features used**: [Social login with Google + auth UI](/blog/2023/04/12/auth-ui), [email sending](/docs/advanced/email)

**UI Framework**: [Chakra UI](https://chakra-ui.com/)

Created in the midst of a GPT craze, this is one of the most popular Wasp apps so far! It does exactly what it says on a tin - given job description and your CV, it generates a unique cover letter customized for you. It does that via parsing your CV and feeding it together with the job description to the GPT api, along with the additional settings such as creativity level (careful with that one!).

Although it started as a fun side project, it seems that people actually find it useful, at least as a starting point for writing your own cover letter. CoverLetterGPT has been used to generate close to 5,000 cover letters!

Try it out and have fun or use it as an inspiration for your next project!

## [Amicus.work](https://www.amicus.work/) - most "enterprise SaaS" app 👔 💼

<ImgWithCaption source="img/build-with-wasp/amicus.png" />

**Try it out**: [amicus.work](https://www.amicus.work/)

**Wasp features used**: [Authentication](/docs/auth/overview), [email sending](/docs/advanced/email), [async/cron jobs](/docs/advanced/jobs)

**UI Framework**: [Material UI](https://mui.com/)

This app really gives away those "enterprise SaaS" vibes - when you see it you know it means some serious business! The author describes it as "Asana for you lawyers" ([you can read how the author got first customers for it here](/blog/2023/02/14/amicus-indiehacker-interview)), or as an easy way for lawyers to manage and collaborate on their workflows.

File upload, workflow creation, calendar integration, collaboration - this app has it all! Amicus might be the most advanced project made with Wasp so far. Erlis startedbuilding it even with Wasp still in Alpha, and it has withstood the test of time since then.

## Description Generator - GPT-powered product description generator - first acquired app made with Wasp! 💰💰

<ImgWithCaption source="img/build-with-wasp/description-generator.png" />

**Try it out**: [description-generator.online](https://description-generator.online/)

**Wasp features used**: [Social login with Google + auth UI](/blog/2023/04/12/auth-ui)

**UI Framework**: [Chakra UI](https://chakra-ui.com/)

Another SaaS that uses GPT integration to cast its magic! Given product name and instructions on what kind of content you'd like to get, this app generates the professionaly written product listing. It's a perfect fit for marketplace owners that want to present their products in the best light but don't have a budget for the marketing agency.

What's special about Description Generator is that it was recently sold , making it the first Wasp-powered project that got acquired! Stay tuned, as the whole story is coming soon.

## TweetBot - your personal Twitter intern! 🐦🤖

<ImgWithCaption source="img/build-with-wasp/tweet-bot.png" />

**Try it out**: [banger-tweet-bot.netlify.app](https://banger-tweet-bot.netlify.app/)

**Source code**: https://github.com/vincanger/banger-tweet-bot

**Wasp features used**:[Authentication](/docs/auth/overview), [async/cron jobs](/docs/advanced/jobs)

**UI Framework**: [Tailwind](https://tailwindcss.com/)

The latest and greatest from [Vince's](https://twitter.com/hot_town) lab - an app that serves as your personal twitter brainstorming agent! It takes your raw ideas as an input, monitors current twitter trends (from the accounts you selected) and helps you brainstorm new tweets and also drafts them for you!

While the previously mentioned projects queried the GPT API directly, TweetBot makes use of the [LangChain](https://js.langchain.com/) library, which does a lot of heavy lifting for you, allowing you to produce bigger prompts and preserve the context between subsequent queries.

## Summary

As you could see above, Wasp can be used to build pretty much any database-backed web application! It is especially well suited for so called "workflow-based" applications where you typically have a bunch of resources (e.g. your tasks, or tweets) that you want to manipulate in some way.

With our built-in deployment support (e.g. you can [deploy to Fly.io for free with a single CLI command](/docs/deployment/deployment-methods/wasp-deploy/fly)) the whole development process is extremely streamlined.

We can't wait to see what you build next!
