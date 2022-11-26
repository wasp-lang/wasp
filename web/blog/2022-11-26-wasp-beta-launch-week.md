---
title: 'Wasp Beta Launch Week announcement'
authors: [matijasos]

image: /img/atp/welcome-to-atp-notion.png

tags: [webdev, wasp, startups, github]
---

import Link from '@docusaurus/Link';
import useBaseUrl from '@docusaurus/useBaseUrl';

import InBlogCta from './components/InBlogCta';
import WaspIntro from './_wasp-intro.md';
import ImgWithCaption from './components/ImgWithCaption'

It’s almost here! After almost two years since our Alpha release, countless apps developed, React and Node versions upgraded, and PRs merged **we’re only a day away from Beta!**

<ImgWithCaption
    alt="Beta is coming"
    source="img/beta-ann/beta-banner.png"
/>

We’re going to follow a launch week format, **which means our Beta launch will last for the whole week**! Starting with the Product Hunt launch this Sunday (we’ll let you know once we’re live, so sharpen your upvoting fingers!) **we’ll highlight a new feature every day**.

I’ll try not to spoil too much in advance but we’re really excited about this - here follows a quick overview of what it’s gonna look like:

<!--truncate-->

## Sunday, Nov 27 - Product Hunt launch event 🚀 + let’s get this party started: **Auth** 🎉

Besides defending our Product Hunt title (we won [#1 Product of the Day](https://www.producthunt.com/products/wasp-lang-alpha#wasp-lang-alpha) last time), this time we’ll also have an online party for all of us to celebrate together!

It will be held **on our Discord at 9:00 am EST / 15:00 CET** - [sign up here](https://discord.gg/4kUcXChX?event=1042717917097246720) and make sure to mark yourself as “Interested”!

Join us to meet the team, attend a relaxed AMA session to learn everything about Wasp, from how it started to development challenges (having fun with Haskell, web dev and compilers) and ideas and plans for the future.

<ImgWithCaption
    alt="Beta launch party instructions"
    source="img/beta-ann/launch-party.png"
/>

The first feature to announce will be authentication in Wasp! It’s easier and cooler than ever, supports 3rd party providers (hint: starts with “G”), and works smoother than a jar of peanut butter (not the crunchy one of course)!

## Monday, Nov 28 - TypeScript support!

<ImgWithCaption
    alt="TypeScript is here!"
    source="img/beta-ann/thank-you-god.gif"
/>

When we asked you what was missing in Wasp during our [Alpha Testing Program](/blog/2022/11/16/alpha-testing-program-post-mortem), you were pretty clear:

<ImgWithCaption
    alt="TypeScript is wanted!"
    source="img/beta-ann/ts-wanted.png"
/>

We heard you (honestly we were missing it too) and now it’s here! You can write your code in TypeScript and enjoy all the goodies that types bring. Some things already work really well and there are a few for which we still have ideas on how to make them better, but more on that on Tuesday!

## Wednesday, Nov 29 - Tailwind support! 🐈 💨

<ImgWithCaption
    alt="Tailwind Nic Cage"
    source="img/beta-ann/nic-cage-tailwind.gif"
/>

It’s beautiful! Another highly anticipated featured that also comes with Beta - support for Tailwind CSS framework! Since it has an additional build step it didn’t work out-of-the-box with Alpha, but now it works like a breeze (see what I did here?)!

Honestly, having used it for designing our new Beta landing page I can really see why it gained so much popularity. So long, making up names for classes, “containers”, and “wrappers”!

## Thursday, Nov 30 - Optimistic updates!

<ImgWithCaption
    alt="Without optimistic updates"
    source="img/beta-ann/no-opt-updates.gif"
    caption="Stop glitching, dang it!"
/>

You know that feeling when you move your Trello card “Try Wasp Beta” from “Todo” column to “Done” column and everything works super smoothly without any glitches? That’s because of optimistic updates! You may not need it often but if you needed and it wasn’t possible you’d feel really sad.

Well, that’s why Alpha is called Alpha and Beta is called Beta 😅. Long story short, now it’s possible to do it in Wasp and it’s also super easy and clean! We're actually very optimistic you’ll feel really good about implementing optimistic updates for your app in Wasp.

## Friday, Dec 1 - Improved IDE support, tooling and Wasp LSP!

<ImgWithCaption
    alt="VS Code support for Wasp LSP"
    source="img/beta-ann/wasp-loves-vscode.png"
/>

If you like types in TypeScript (and in general), then you will also enjoy Wasp! Our DSL is also a typed language which means it can report errors in compile time, e.g. in case you haven’t configured your route correctly. And now all that happens directly in your editor!

**Beta brings LSP, Language Server for Wasp that works with VS Code** (support for other editors coming soon! I’m VIM user myself so take a guess :D). That means improved syntax highlighting, code autocompletion and live error reporting - everything you’d expect from a language!

<ImgWithCaption
    alt="Wasp Language Server in action"
    source="img/beta-ann/wls-demo.gif"
    caption="Wasp LSP in action!"
/>

## Saturday, Dec 2 - Grande Finale + #1 Wasp Hackathon!(Waspathon🐝 ?)

<ImgWithCaption
    alt="First Wasp hackathon"
    source="img/beta-ann/hackathon-banner.gif"
/>

I don’t want to reveal too much in advance, but yep there will be a hackathon, yep there will be cool rewards (at least we think so) and yep it will be awesome! We’ll officially announce it as we end the launch week, and equipped with all the new features Beta brought we’ll switch into the hacking mode!

It’s our first hackathon and we can’t wait to tell you more about it (ok, I admit, we’re still working on it) and see what you beeld with Wasp!

## Recap

- **We are launching Beta this Sunday, Nov 27, on Product Hunt at 1am PST / 4am EST / 10am CET** - make sure to upvote and comment (anything counts, even “go guys!”) when you can
- **Beta brings a ton of new exciting features** - we’ll highlight one each day of the following week
- **On Saturday, Dec 2, we’ll announce a hackathon** - our first ever!

That’s it, Waspeteers - keep buzzing as always and see you soon on the other side! 🐝  🅱️

Matija, Martin & the Wasp team
