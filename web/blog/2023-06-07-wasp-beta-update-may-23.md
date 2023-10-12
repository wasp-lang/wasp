---
title: 'Wasp Beta - May 2023'
authors: [matijasos]
image: /img/update-may-23/banner.png
tags: [update]
---

import Link from '@docusaurus/Link';
import useBaseUrl from '@docusaurus/useBaseUrl';

import InBlogCta from './components/InBlogCta';
import WaspIntro from './_wasp-intro.md';
import ImgWithCaption from './components/ImgWithCaption'

<ImgWithCaption
    alt="Wasp Update May 23"
    source="img/update-may-23/banner.png"
/>

*Want to stay in the loop? → [Join our newsletter!](/#signup)*

Hola Waspeteers 🐝,

What did one plant say to the other? Aloe! Long thyme no see. 🌱

Now that we've set the tone, let me guide you through what's new in Waspworld (that would be a cool theme park, right?):

<!--truncate-->

## Wasp Hackathon 2.0 is over - congrats to the winners! 🐝 🏆 🐝

<ImgWithCaption
    alt="Congrats to the hackathon winners!"
    source="img/update-may-23/typergotchi.png"
    caption="Shoutout to the winning team - Typergotchi! They even made a cool illustration with our mascot, Da Boi 🐝 😎"
/>

We had more submissions than ever, and the quality and creativity of your apps were really at the next level. We had everything from admin dashboards and GPT-powered story-telling apps to the actual games.

<ImgWithCaption
    alt="Hackathon testimonial"
    source="img/update-may-23/testimonial-hackathon.png"
/>

See all the winners and read a full Hackathon 2.0 review [👉 here 👈](/blog/2023/05/19/hackathon-2-review).

## Wasp Launch Week #3 is in the making - get ready for the Magic 🔮 🧙

As it always happens in the wilderness, after one launch week, there comes another one. And who are we to defy the laws of nature - thus, get ready for Launch Week #3!

We are aiming for the end of June, but we'll announce the exact date soon. Make sure to [follow us on Twitter](https://twitter.com/WaspLang) or/and [join our Discord](https://discord.gg/rzdnErX) to stay in the loop.

<ImgWithCaption
    alt="Beautiful"
    source="img/update-may-23/beautiful.gif"
    caption="When you see it ✨"
/>

### After Pizzazz 🍕 ...
As you might remember, the motto/topic of our [last launch](/blog/2023/04/11/wasp-launch-week-two) was *Pizzaz*, which referred to improving the developer experience in Wasp - full-stack auth, one-line deployment, type safety, db tooling, ...

### ... Comes Magic! 🔮
While DX will always be our top priority, we're now shifting gears a bit - the keyword we chose to represent our next launch is ✨ *Magic* ✨. The reason is that now that we have a majority of the features you'd expect in a web framework in place, **we can start utilizing Wasp's unique compiler-driven approach to offer next-level features no other framework can!**

### LW3 Sneak Peek 🤫 👀
More details coming soon, but in the meanwhile, here are some of the features we're most excited about:

#### 🚧 Wasp AI 🤖 ✨
There is no mAgIc without AI! We cannot share many details on this yet, but it is something we've been exploring a lot lately. Our previous experiments have shown that, due to its declarative and human-readable nature, Wasp is naturally a very good fit for LLMs.

We'll take this to the next level for our next launch - stay tuned!

#### 🚧 Auto CRUD
Although Wasp helps a lot with bootstrapping your app, one repetitive thing that you have to do every time is implement "standard" CRUD operations for your data models.

We decided to put a stop to it - welcome our new (incoming) feature, Auto CRUD!

<ImgWithCaption
    alt="Auto CRUD"
    source="img/update-may-23/auto-crud.png"
    caption="Syntax proposal for the new Auto CRUD feature"
/>

All you have to do is specify in your .wasp file which CRUD operations you want, and they will be auto-generated for you to use in your JS/TS code. **The best part is when you update your data model, these will get updated as well! 🤯**

This feature is also a really good showcase of Wasp's compiler muscles - the best you could get with a traditional framework approach is scaffolding, which means spitting out code that will quickly get outdated and that you have to maintain.

[See a 2-min demo of Wasp Auto CRUD in action](https://www.youtube.com/watch?v=IzBxpqV5USE&ab_channel=Wasp) - by our founding engineer Miho

<ImgWithCaption
    alt="Showing off compiler muscles"
    source="img/update-may-23/muscles.gif"
    caption="Our compiler right now"
/>

#### 🚧 Advanced syntax completion for .wasp files (LSP)

<ImgWithCaption
    alt="Improved LSP"
    source="img/update-may-23/new-lsp.png"
/>

We're making our [VS Code extension](https://marketplace.visualstudio.com/items?itemName=wasp-lang.wasp) even better! So far it has provided highlighting and auto-completion for top-level declarations (e.g., *route, entity, query*, ...), but now it's going even deeper. Every property will display its full type as you are typing it out + you'll get a context-aware auto-completion. 


#### 🚧 Support for web sockets 🔌 🧦!
Wasp will soon support Web Sockets! This will allow you to have a persistent, real-time connection between your client and server, which is great for chat apps, games, and more.

<ImgWithCaption
    alt="Web sockets in Wasp"
    source="img/update-may-23/web-sockets.png"
    caption="Defining a new web socket in Wasp config file"
/>

For now it is a stand-alone feature, but it opens some really interesting possibilities - e.g. combining this with Wasp's query/action system and letting you declare a particular query to be "live". Just an idea for now but something to keep in mind as we test and receive more feedback on this feature.

## From the blog 📖
- [How I Built CoverLetterGPT - SaaS app with the PERN stack, GPT, Stripe, & Chakra UI](/blog/2023/04/17/How-I-Built-CoverLetterGPT) - our one and only Vince shares the story behind CoverLetterGPT; how he built it and got thousands of users in just a few weeks
- [Hackathon #2: Results & Review](/blog/2023/05/19/hackathon-2-review) - see how our hackathon went and what the contestants think about Wasp!

## The community buzz 🐝 💬
Last month was super buzzy! We got several awesome reviews, and Wasp also got picked up by a couple of YouTube dev influencers:

<ImgWithCaption
    alt="Wasp testimonial"
    source="img/update-may-23/testimonial-jon.png"
/>

- 🎬 [Build Full-Stack Notes App w/ Wasp! (ReactJS, Prisma, Node)](https://www.youtube.com/watch?v=AA4ckj1P5QY&t=12s&ab_channel=webdecoded) - an amazing 30-min overview of how to build a classic notes app with Wasp
- 🎬 [Live Coding Serverlesspresso with WaspLang!](https://www.youtube.com/watch?v=c3-bbzrdC8E&ab_channel=DevAgrawal) - 4-hour session of live coding a coffee ordering app. Only for the most hardcore folks plus you'll need a lot of coffee :D
- ✒️ [Wasp Configuration Language: Simplify Full Stack App Creation](https://blog.oleggulevskyy.dev/wasp-configuration-language-simplify-full-stack-app-creation) - a lightweight overview of Wasp and why the author prefers it to Next/T3
- ✒️ [Wasp(athon) impressions & some proposals](https://medium.com/@umbrien/wasp-athon-impressions-some-proposals-8f3726890009) - from one of our hackathon contestants - first impressions and spot-on ideas for the next features

## Wasp GitHub Star Growth - 2,825 ⭐
Getting close to the big 3,000! Huge thanks to all our contributors and stargazers - you are amazing!

<ImgWithCaption
    alt="GitHub stars - almost 3,000!"
    source="img/update-may-23/github-stars.png"
    caption="Almost 3,000 stars! 🐝 🚀"
/>

And if you haven't yet, please [star us on Github](https://wasp-lang.dev/docs)! Yes, we are shameless star beggars, but if you believe in the project and want to support it that's one of the best ways to do it (next to actually building something with Wasp - [go do that too](https://wasp-lang.dev/docs)! :D)

## That's a wrap! 🌯

<ImgWithCaption
    alt="A dramatic goodbye gif"
    source="img/update-may-23/goodbye-dramatic.gif"
    caption="A dramatic goodbye - don't ever let go"
/>

That's it for this month and thanks for reading! Since you've come this far, you deserve one final treat - a Wasp-themed joke generated by ChatGPT:

<ImgWithCaption
    alt="GPT Wasp joke"
    source="img/update-may-23/gpt-wasp-joke.png"
    caption="Good one, dad."
/>

Fly high, and we'll see you soon 🐝 🐝,  
Matija, Martin and the Wasp team


