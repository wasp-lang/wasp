---
title: 'How I Built CoverLetterGPT - SaaS app with the PERN stack, GPT, Stripe, & Chakra UI'
authors: [vinny]
image: /img/cover-letter-gpt-yt.png
tags: [wasp, ai, gpt, fullstack, PERN, stripe, chakra, saas]
---
import Link from '@docusaurus/Link';
import useBaseUrl from '@docusaurus/useBaseUrl';

import InBlogCta from './components/InBlogCta';
import WaspIntro from './_wasp-intro.md';
import ImgWithCaption from './components/ImgWithCaption'

<br/>
<div style={{ textAlign: "center", width: "100%", height: "400px", display: "inline-block" }}>
<iframe height="100%" width ="100%" src="https://www.youtube.com/embed/D1l0iwGUed0" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen="true"></iframe>
</div>

<!--truncate-->

---

Like many other software developers, I enjoy trying out new technologies even if it's just to get a feel for what they can do. 

So when I first learned about the [OpenAI API](https://platform.openai.com/docs/api-reference), I knew I wanted to give it a try. I had already wanted to create a SaaS app that could help manage the process of applying to numerous jobs, and the prospect of adding GPT into the mix made it even more interesting. So with API access and a bit of free time, I decided to give it a shot.

I threw together a simple version of the app in about 3-4 days and [CoverLetterGPT](https://coverlettergpt.xyz) was born, a SaaS app that uses GPT-3.5-turbo to generate, revise, and manage cover letters for you based on your skills and the specific job descriptions. 

Even though I did think it had potential as a SaaS app, I was approaching it mostly as a way to learn how to build one for the first time. And after seeing so many people "building in public" and sharing their progress, I thought it would be fun to try it out myself.

<div style={{ marginBottom: "1rem" }}>
    <a href="https://twitter.com/hot_town/status/1633873684573323265?s=20">
        <img src={useBaseUrl('img/coverlettergpt.png')} alt="Hey peeps. Check out http://coverlettergpt.xyz. You can try it out now and create your own cover letters for free (no Payment/API key). I'm working on A LOT more features. Stay Tuned!"/>
    </a>
</div>


So I started sharing my progress on Twitter, Reddit, and Indie Hackers. I made my first post about it on March 9th, and because I was just experimenting and trying my hand at a SaaS app for the first time, I also [open-sourced the app](https://github.com/vincanger/coverlettergpt) to share the code and what I was learning with others. This led to a lot of interest and great feedback, and I ended up getting featured in the [indiehackers newsletter](https://www.indiehackers.com/post/whats-new-don-t-build-things-no-one-wants-833ee752ba?utm_source=indie-hackers-emails&utm_campaign=ih-newsletter&utm_medium=email), which led to even more interest.

Within the first month, I got over 1,000 sign-ups along with my first paying customers. Pretty surprising, to say the least!

So to continue in the spirit of curiosity, learning, and just "wingin' it," I decided to make a code walkthrough video that explains how I built the app, the tools I used to build it, and a little bit about how I marketed the app without spending any money. 

As an extra bonus, I also give a quick introduction to the [free SaaS template](https://github.com/wasp-lang/SaaS-Template-GPT) I created for building your own SaaS app, with or without GPT, on the PERN stack (PostgreSQL/Prisma, Express, React, NodeJS).

My hope is that others will learn something from my experience, and that it could inspire them to try out new technologies and build that app idea they've had in mind (and if they do, they should make sure to share it with me on Twitter [@hot_town](https://twitter.com/hot_town) -- I'd love to see it!)


*Want to stay in the loop? → [Join our newsletter!](https://wasp-lang.dev/#signup)*
