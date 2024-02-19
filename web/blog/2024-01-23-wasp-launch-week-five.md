---
title: 'Wasp Launch Week #5: Waspnado 🐝 🌪️'
authors: [matijasos]
image: /img/lw5/lw5-banner.png
tags: [launch-week, update]
---

import Link from '@docusaurus/Link';
import useBaseUrl from '@docusaurus/useBaseUrl';

import InBlogCta from './components/InBlogCta';
import WaspIntro from './_wasp-intro.md';
import ImgWithCaption from './components/ImgWithCaption'

<ImgWithCaption
    alt="Launch Week 5 is here"
    source="img/lw5/lw5-banner.png"
/>

New Year, New Wasp! That's we at first wanted to use as motto for this launch, but then I bought a DALL-E subscription and typed in "*Waspnado, but with plushies*". The rest is history.

**TL;DR** - Wasp is getting dangerously close to 10,000 stars on GitHub and our Discord community is shy of 2,000 members! We're seeing more and more users building and deploying cool apps, both AI-powered, but also good old SaaS-es. Another thing we still have to get used to is getting nice messages from you, such as this one: 

<ImgWithCaption
    alt="Nice testimonial"
    source="img/lw5/nice-testimonial.png"
/>

Thank you! This means a lot to us and reassures our that we are on the right path. Now, without the further ado, let's dive in and see what awaits us this week (a tornado of new features, of course):

## Day 1: Wasp Auth 2.0

[Auth](/docs/auth/overview) is one Wasp's flagship, and most popular features. All you need to do is add providers you want to use in Wasp config file (e.g. email, Google, or GitHub) and poof - **Wasp will magically create a full-stack auth for you, from the database models to the UI components that you can simply import and use**. Here's an 1-minute tour:

<div className='video-container'>
    <iframe src="https://www.youtube.com/embed/Qiro77q-ulI?si=y8Rejsbjb1HJC6FA" frameborder="1" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>
</div>

And the best part - **this all works without any 3rd party services**! This is all your code that runs on your infrastructure, and you don't have to pay for it, no matter how many users you have. Pretty neat, huh?

This is all old news, so what's new? I won't spoil too much, but we might have made it even easier to use (no more manually defining data models), plus it now might use a popular library that starts with "*L*" (and ends with "*ucia*") under the hood. I won't say anything more!

**Read more about it:**
- [Twitter thread introing Wasp Auth 2.0](https://twitter.com/WaspLang/status/1749830729448481254)

## Day 2: Wasp, restructured - `package.json` is back! 🏗️

<ImgWithCaption
    alt="Pinocchio"
    source="img/lw5/real-framework.gif"
    caption="Yes, Wasp, you are a real framework now!"
/>

This is a big one, and the most complex feature we'll be shipping this week! We've been designing Wasp from day 1 to work nicely with other pieces of the stack, such as React and Node.js. But, some decisions we made in the process on how all these work together weren't the most elegant, both for you as users and us as developers of the framework. 

With these changes, **Wasp will feel much more like a "real" framework that you are used to** - you will be able to use `npm install`, access your `package.json`, `tsconfig`, and more!

## Day 3: Wasp AI aka MAGE now lives in your CLI! 🤖 📟

[MAGE](https://usemage.ai/) is an AI-powered, full-stack, React and Node.js web app generator powered by Wasp. All it takes is writing a short description and that's it - you will get a complete codebase you can download, run locally, adjust to your wishes and deploy!

It is one of our most successful products [that has been used to kickstart over 30,000 applications](https://dev.to/wasp/how-we-built-a-gpt-web-app-generator-for-react-nodejs-from-idea-to-25000-apps-in-4-months-1aol)!

<ImgWithCaption
    alt="MAGE in action - browser"
    source="img/lw5/mage-hiw.gif"
/>

So far, you could access MAGE only through it's web interface, hosted at https://usemage.ai/. This is super handy to get started quickly, but what if you already have Wasp installed on your computer, or you want more control over the generation (e.g. use GPT-4 exclusively)? That's when running MAGE from you CLI comes into play! Here's how it works:

<div className='video-container'>
    <iframe src="https://www.youtube.com/embed/0cuNS3ji_II?si=QfM9nWHH9zWFuNIU" frameborder="1" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>
</div>

This is also the foundation for adding more advanced features to MAGE in the future, like interactive debugging.

## Day 4: Open SaaS - freedom to the boilerplate!

<ImgWithCaption
    alt="Open SaaS revolution"
    source="img/lw5/open_saas_freedom.png"
/>

Remember seeing the boilerplate starters costing more than $300, just to start your side project, and then you still have to maintain all that code? Well, we do, and we say no more!

All the best things in the world are free, and there aren't much better things than **a feature-rich, production-grade boilerplate starter for React & Node.js with admin dashboard, Stripe and OpenAI integration and more - 100% free and open source!** (love is a close second)

You can check it out at https://opensaas.sh/ and give it a star on https://github.com/wasp-lang/open-saas - more details coming soon!

## Day 5 - New Year, New Wasp!

<ImgWithCaption
    alt="Say my name"
    source="img/lw5/say-my-name.gif"
/>

On the last day of the Launch Week, we're not presenting another feature, but rather a new brand for Wasp (don't worry, Da Boi stays). It will be shorter, sleeker and even easier to remember. Stay tuned and see what this is all about!

## Stay in the loop

<ImgWithCaption
    alt="dont leave"
    source="img/lw5/dont-leave.gif"
/>

Every day, we'll update this page with the latest announcement of the day - to stay in the loop, [follow us on Twitter/X](https://twitter.com/WaspLang) and [join our Discord](https://discord.gg/rzdnErX) - see you there!

