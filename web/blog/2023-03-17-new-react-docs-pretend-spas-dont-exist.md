---
title: "New React docs pretend SPAs don't exist anymore"
authors: [matijasos]
image: img/new-react-docs/evan-you-no-vite.png
tags: [webdev, react] 
---

import Link from '@docusaurus/Link';
import useBaseUrl from '@docusaurus/useBaseUrl';

import InBlogCta from './components/InBlogCta';
import WaspIntro from './_wasp-intro.md';
import ImgWithCaption from './components/ImgWithCaption'

<ImgWithCaption
    alt="Where is Vite"
    source="img/new-react-docs/evan-you-no-vite.png"
/>

React just released their new docs at [https://react.dev/](https://react.dev/). While it looks great and packs a lot of improvements, one section that caught the community’s attention is “[Start a New React Project](https://react.dev/learn/start-a-new-react-project)”. The strongly recommended way to start a new React project is to use a framework such as Next.js, while the traditional route of using bundlers like Vite or CRA is fairly strongly discouraged.

Next.js is a great framework, and it’s rise in popularity is due in a large part to the return of SEO optimization via Server-Side-Rendering (SSR) within the collective developer conscience. And it definitely does make sense to use a framework that provides SSR for static sites and pages that rely on SEO.

But what about typical Single Page Apps (SPAs)? Dashboard-like tools that live behind the auth (and don’t need SEO at all), and for which React was originally designed, still very much exist.

<!--truncate-->

## The new React docs - use a framework unless your app has “unusual” constraints

<ImgWithCaption
    alt="react new project docs"
    source="img/new-react-docs/react-new-project.png"
/>

The new docs make a pretty strong claim for using a framework when starting a new React project. Even if you read through the “Can I use React without a framework” section (hidden behind a collapsed toggle by default), you have to go through a wall of text convincing you why not using a framework is a bad idea, mainly due to the lack of SSR. Only then, in the end, comes the piece mentioning other options, such as Vite and Parcel:

<ImgWithCaption
    alt="use framework unless you app has unusual constraints"
    source="img/new-react-docs/your-app-unusual.png"
/>

Even then, first you’ll have to admit your app has unusual constraints (and no examples were given of what that could be) before you’re actually “allowed” not to use a framework. It feels very much like you’re doing it in spite of all the warnings and that there actually isn’t a case where you should do it.

## Why SPAs (still) matter

<ImgWithCaption
    alt="SPAs still have their place"
    source="img/new-react-docs/spas-have-place.png"
/>

SSR/SSG has been getting a lot of attention lately and has been a flagship feature of most new frameworks built on top of React. And rightly so - it has solved a major issue of using React for static & SEO-facing sites where time to first content (FCP) is crucial.

On the other hand, the use case where React, Angular, and other UI frameworks initially shined were dashboard apps (e.g., project management systems, CRMs, …) - it allowed for a radically better UX, which resembled that of desktop apps.

Although interactive content-rich apps (blogging platforms, marketplaces, social platforms) are today a typical poster child demo app for frameworks, dashboard-like apps still very much exist, and there are more of them than ever. Thousands of companies are building their internal tools daily, just like new SaaS-es pop up every day.

SEO is largely irrelevant for them since everything is happening behind the auth layer, where everything is centered around workflows, not content. SSR might even be counter-productive since it puts more pressure on your servers instead of distributing the rendering load across the clients.

## How then would you develop SPAs?

Traditionally, React was only a UI library in your stack of choice. You would use CRA (or Vite nowadays) as a bundler/starter for your React project. Then you’d probably add a routing library (e.g., react-router) and maybe a state management library (e.g., Redux, or react-query), and you’d already be set pretty well. You would develop your backend in whatever you choose - Node.js/Express, Rails, or anything else.

There are also new frameworks emerging that focus on this particular use case (e.g., [RedwoodJS](https://redwoodjs.com/) and [Wasp](https://wasp-lang.dev) (disclaimer: this is us!)) whose flagship feature is not SSR, but rather the abstraction of API and CRUD on data models, and getting full-stack functionality from UI to the database, with extra features such as easy authentication and deployment out of the box.

With a “go for Next or you are unusual” and “you need SSR” message, React is making a strong signal against other solutions that don’t emphasize SSR as their main feature.

## So what’s the big deal? Nobody forces you to use SSR in Next/Remix

That’s correct, but also it’s true that a buy-in into a whole framework is a much bigger step than just opting for a UI library. Frameworks are (more) opinionated and come with many decisions (code structure, architecture, deployment) made upfront for you. Which is great and that’s why they are valuable and why we’ll keep using them.

But, both sides of the story should be presented, and the final call should be left to the developer. React is too useful, valuable, and popular a tool and community to allow itself to skip this step.


*Want to stay in the loop? → [Join our newsletter!](/#signup)*

