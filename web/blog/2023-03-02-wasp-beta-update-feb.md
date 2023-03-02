---
title: 'Wasp Beta - February 2023'
authors: [matijasos]
image: /img/update-feb-23/banner.png
tags: [webdev, wasp, startups, github]
---

import Link from '@docusaurus/Link';
import useBaseUrl from '@docusaurus/useBaseUrl';

import InBlogCta from './components/InBlogCta';
import WaspIntro from './_wasp-intro.md';
import ImgWithCaption from './components/ImgWithCaption'

<ImgWithCaption
    alt="Wasp Update Feb 23"
    source="img/update-feb-23/banner.png"
/>

*Want to stay in the loop? → [Join our newsletter!](/#signup)*

Hey Wasp acolytes (Waspolytes?) 🐝,

What's kickin'? We at Wasp spent the whole month thinking of the coolest features to add to our next release and we can't wait to share it with you!

<!--truncate-->

<ImgWithCaption
    alt="Tell me now"
    source="img/update-feb-23/tell-me-now.gif"
    caption="Ok ok, we're getting there, chill!"
/>

Let me cut to the chase and show you what's been cooking in Wasp pot for the past month:

## Deploy to Fly.io with a single command for free 🚀☁️

<ImgWithCaption
    alt="Deploy to fly.io with single command"
    source="img/update-feb-23/deploy-fly.png"
/>

This is the only command you need to run to deploy your full app (client, server, and database) to Fly.io! They also offer a generous free tier so you can deploy your v1 without any second thoughts.

Check out our docs for more details: [Deploying your Wasp app to Fly.io](/docs/deploying#wasp-cli)

## ✅ Full stack TypeScript support

<ImgWithCaption
    alt="Types everywhere"
    source="img/update-feb-23/types-buzz.png"
/>

This is one of the features we are most excited about! Now, when you define an entity in your Wasp file, it immediately becomes accessible as a type both on a client and a server.

<ImgWithCaption
    alt="Full stack TypeScript support"
    source="img/update-feb-23/fullstack-ts.png"
/>

This feature beautifully showcases the power of the Wasp language approach and how much it can cut down on the boilerplate. And we're just getting started!

## 🗓 We set a date for the next launch - April 11th! 🚀

<ImgWithCaption
    alt="Launch party"
    source="img/update-feb-23/ballmer-launch.gif"
/>

Mark your calendars, it's official! **We will release the next version of Wasp on April 11th - in exactly 40 days**! As the last time, we will follow a launch week format with a lot of memes, swag and fun prizes (Including Da Boi, of course).

That's why we introduced [Wasp GitHub Discussions](https://github.com/wasp-lang/wasp/discussions)! It's a relatively new service by GitHub that allows distinguishing between specific, well-defined issues (bug reports, TODOs, ...) and discussion items (ideating about new features, figuring out best practices, etc) and allows for upvotes from the community.

Here's a quick list of the planned features:
- Using Vite instead of CRA under the hood - you'll be able to create new Wasp apps in a blink of an eye! 🚀
- Custom API routes
- Code scaffolding for the quicker start
- Support for sending emails
- Password reset via email
- Improved Auth UI
- Testing support

And more! This is quite an ambitious plan but we are fully committed to getting it done. Any comments or ideas, [ping us on our Discord](https://discord.gg/rzdnErX).

## ☎️ We had our Community Call #2 - meet Da Boi

<ImgWithCaption
    alt="We had a community call"
    source="img/update-feb-23/da-boi-call.jpeg"
/>

We had so much fun on our last community call that we decided we have to do it again! As you can notice, our community-approved mascot Da Boi stole the show. The rest was pretty much just a filler and an excuse to have more fun with Da Boi :D.

On a serious note, it was great to catch up with the community prior to the next release - we discussed features and the roadmap and everybody shared what they're building and what they'd like to see next in Wasp.

## 🎥 Wasp is now on YouTube!

<ImgWithCaption
    alt="Wasp is on YouTube"
    source="img/update-feb-23/yt-junior-devs.png"
/>

We are still going strong with our YouTube! **The [latest video](https://youtu.be/eermNn9VhOA) started as a question on Reddit and it escalated quite quickly, with 200+ comments** - we cover the responses we received + our expert commentary :D.

If you want to stay in the loop (and I guess you do since you're reading this :D), please [subscribe to our channel](https://www.youtube.com/@wasplang) and help us reach the first 100 subscribers on YouTube!

<ImgWithCaption
    alt="Subscribe to Wasp on YouTube"
    source="img/update-dec-23/yt-cta.png"
    caption="You know you want it!"
/>

## ⌨️ From the blog

* [The Best Web App Framework Doesn't Exist](/blog/2023/02/02/no-best-framework) - or this one we invented our own "Hierarchy of Developer Needs". Scientifically proven 🤞.

* [The Most Common Misconceptions Amongst Junior Developers](/blog/2023/02/21/junior-developer-misconceptions) - we asked and Reddit answered!

## 🕹 Community highlights

* [PhraseTutor: Learn Italian in a week](https://phrasetutor.com/)! There is a new app built from scratch with Wasp, by Mihovil - one of our early community members who recently joined the team as an engineer! It's smooth both on the front end and back end and will teach you Italian before you can say (or eat) "quattro formaggi"!
<ImgWithCaption
    alt="Phrase Tutor"
    source="img/update-feb-23/phrase-tutor.png"
/>


## Developer life 💻⌨️💽
Here is the cool stuff we came across this month

- [Motion Canvas - Visualize complex ideas programmatically](https://motioncanvas.io/) - a very cool 2d animation library and editor! I want to play around with this one so much and create a cool animation for Wasp (Da Boi anyone?) 😄
<ImgWithCaption
    alt="Motion Canvas"
    source="img/update-feb-23/motion-canvas.png"
/>

- [98.css - A design system for building faithful recreations of old UIs](https://jdan.github.io/98.css/) - I think we can all agree that web design is becoming increasingly minimalistic and getting rid of the flashy gradients and shadows. Be ahead of the curve - make your app look like Windows 98! You'll instantly feel 10 (20?) years younger and get an irresistible urge to play Minesweeper 🚩.
<ImgWithCaption
    alt="98 css"
    source="img/update-feb-23/98-css.png"
/>

- [Squeezing a Sokoban game into 10 lines of code](https://www.cole-k.com/2023/02/21/tiny-games-hs/) - this is why Haskell gets a bad rep and why we can't have nice things 😅 I'll urge you to look away, but I know you won't. Jokes aside, this was cool! The blog post is also really well written and it was super fun to read.
<ImgWithCaption
    alt="Sokoban in 10 lines of code"
    source="img/update-feb-23/sokoban-10loc.png"
/>


## Wasp Github Star Growth - 2,317 ⭐️, woohoo!
Huge thanks to all our [contributors](https://github.com/wasp-lang/wasp/graphs/contributors) and [stargazers](https://github.com/wasp-lang/wasp/stargazers) - you are amazing!

<ImgWithCaption
    alt="Wasp has over 2,000 GitHub stars"
    source="img/update-feb-23/gh-stars.png"
/>

And if you haven't yet, please [star us on Github](https://wasp-lang.dev/docs)! Yes, we are shameless star beggars, but if you believe in the project and want to support it that's one of the best ways to do it (next to actually building something with Wasp - [go do that too](https://wasp-lang.dev/docs)! :D)

That's a wrap! Thanks for reading and we can't wait for our next launch to get out and see how you like it. As always, [we're on Discord](https://discord.gg/rzdnErX) and appreciate any comments, feedback, and ideas - that's how Wasp came to be!

As a parting gift, here are a few curated Da Boi memes created by our valued community members:

<ImgWithCaption
    alt="Wasp's new mascot"
    source="img/update-feb-23/wasp-discord-meme.jpeg"
/>

Buzzy buzz, you got that snazz 🐝 🐝,  
Matija, Martin and the Wasp team


*Want to stay in the loop? → [Join our newsletter!](/#signup)*

