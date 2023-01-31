---
title: 'Convincing developers to try a new web framework - the effects of launching beta'
authors: [matijasos]
image: /img/beta-launch-review/beta-feedback.png
tags: [webdev, wasp, startups, github]
---

import Link from '@docusaurus/Link';
import useBaseUrl from '@docusaurus/useBaseUrl';

import InBlogCta from './components/InBlogCta';
import WaspIntro from './_wasp-intro.md';
import ImgWithCaption from './components/ImgWithCaption'


<ImgWithCaption
    alt="Alpha feedback"
    source="img/beta-launch-review/alpha-feedback.png"
    caption="How it started: feedback on Wasp Alpha, 3 years ago"
/>

We are developing an OSS web framework in a form of a config language (DSL) that works with React & Node.js. Getting developers to use a new tool (especially a web framework) is [a pretty hard thing to do](/blog/2022/09/29/journey-to-1000-gh-stars). We wished there were more stories of how today's mainstream tools got adopted that we could learn from, so that motivated us to document our own.

*Want to stay in the loop? → [Join our newsletter!](/#signup)*

<!--truncate-->

## TL;DR
- [HackerNews launch](https://news.ycombinator.com/item?id=33910997) post brought the most traffic, by far
- Product Hunt launch was worse then expected, bots took over
- Our goal was to reach GitHub Trending but we failed
- Less traffic overall than for the Alpha launch, but much higher quality of feedback + change of public perception
- Having a public launch date made us 3x more productive

## 📊 The results: stats
We [launched Beta](http://localhost:3000/blog/2022/11/29/wasp-beta) on Nov 27, 2022 in a launch week format, recently popularized by [Supabase](https://supabase.com/blog/supabase-how-we-launch). During the first week we launched on Product Hunt, and after the weekend we posted on HackerNews. Here's what the numbers were on the last day of the launch:
- 190 GitHub stars added to the repo
- 108 new projects started
- 83 new users (installed Wasp locally and ran it)

<ImgWithCaption
    alt="Web visitors during beta launch week"
    source="img/beta-launch-review/launch-week-visitors.png"
/>

HN launch caused almost 2x spike in traffic and usage. Also, although our launch week already ended by the start of December, we actually had the most users ever throughout December:

<ImgWithCaption
    alt="WAU displayed monthly"
    source="img/beta-launch-review/wau-monthly.png"
/>

Looking back, this wasn't at all our biggest event in terms of traffic, but it was in terms of usage:

<ImgWithCaption
    alt="All time stats"
    source="img/beta-launch-review/all-time-stats.png"
/>

One of the main effects of the launch (together with a [few](https://news.ycombinator.com/item?id=32098144) [recent](https://news.ycombinator.com/item?id=32566123) successful HN posts, and the [Alpha Testing Program](/blog/2022/11/16/alpha-testing-program-post-mortem) we ran in Jul '22) is that we managed to move the baseline WAU from ~10 to ~20. Another effect, felt more subjectively, is the change in the community perception (I will say more about it below).

## Before the launch
This was our initial plan:
<ImgWithCaption
    alt="Launch timeline"
    source="img/beta-launch-review/launch-timeline.png"
/>

For 20 days before the launch we were posting daily countdown banners on Twitter + a few polls (e.g. what's your favourite CSS framework) to engage the audience.

<ImgWithCaption
    alt="Examples of pre-launch tweets"
    source="img/beta-launch-review/prelaunch-tweets.png"
/>

[Our Twitter game](https://twitter.com/WaspLang) is still super young (~500 followers) so it didn't have a big effect but it helped to get the team excited and a few people also noticed it and commented/voted.

Due to the lack of time we ended up doing user testing in-house. That's still something I'd like to improve and make a habit of in the future.

A few other things we did prior to the launch:
- **Redesigned our project page** - gave it a new, sleeker look
- **Published [use cases with our most successful users](/#showcases)** and featured them on the project page
- **Activated our Discord and [email list](https://us4.campaign-archive.com/?u=8139c7de74df98aa17054b235&id=1195fce664)**
- **Organized a launch event** (call on Discord) to celebrate the launch - it went better than expected, a decent amount of people showed up and we had some good discussions!

## The launch
As mentioned, we went with a launch week format - we liked the idea of having a whole week filled with content rather than cramming everything in a single day. We highlighted a new feature every day + launched a hackathon on the last day of the week, to keep the momentum. You can see the full schedule [here](/blog/2022/11/26/wasp-beta-launch-week).

<ImgWithCaption
    alt="Launch week schedule"
    source="img/beta-launch-review/launch-schedule.png"
/>

We also shared our launch news at different places, most successful being [Product Hunt](https://www.producthunt.com/products/wasp-lang-alpha#wasp-lang-beta), [HackerNews](https://news.ycombinator.com/item?id=33910997) and [Reddit](https://www.reddit.com/r/javascript/comments/z7xo9t/wasp_dsl_framework_for_building_fullstack_js/).

## Product Hunt - failed, but ok
The mistake we did was launching on the Thanksgiving weekend - there was little (real) traffic + the mods were away so the bots took over!

We ended up as [#5 product of the day](https://www.producthunt.com/products/wasp-lang-alpha#wasp-lang-beta) with ~250 upvotes, which wasn’t so bad because in the end we got featured in their daily newsletter with 1M+ subscribers.

The bad part was that mods were away and **pretty much all other products in front of us were fake or obviously bot powered!** It felt like there was no real interaction on any of these products, just endless “congrats on the launch” comments from the newly created accounts with obviously fake names. Two products were also clearly violating PH rules (one was the same product that launched a week or two ago, but just changed the name).

The most disappointing part for us (and especially for the team) was that it felt like there aren’t any real people on PH, just bots.

## 🕹 Post-launch: Wasp Hackathon #1 - Betathon!
Since we introduced all the new features during the launch week, we thought a good way to keep the community engaged and give them a reason to try Wasp Beta out would be to throw a hackathon! It was the first time we did so we weren't sure how it'd go, but it went better than expected!

<ImgWithCaption
    alt="Tweet about Betathon - our #1 hackathon!"
    source="img/beta-launch-review/hackathon-tweet.png"
/>

In the end, it was definitely worth it ([see review and submissions here](https://wasp-lang.dev/blog/2023/01/11/betathon-review)). It was quite lightweight to organize (we even made a [custom web app with Wasp for the hackathon](https://betathon.wasp-lang.dev/) which you can also [use for your hackathon](https://github.com/vincanger/wasp-betathon)) and we got some really nice submissions and community shout-outs.

## Community perception shift
As mentioned above, although our Alpha launch had higher absolute numbers (website traffic, HN upvotes etc), **it felt that Beta launch caused the biggest perception shift in the community so far**.

Before were mostly getting superficial comments like *“this looks cool, I’ll give it a try once”*, or *“why DSL approach and not the other one”*, and this time we could notice that portion of people already knew Wasp from before (some even used it), and had more specific questions, even proposing next features that we planned but haven’t published yet.

<ImgWithCaption
    alt="Beta feedback"
    source="img/beta-launch-review/beta-feedback.png"
/>

Although the core message (DSL for developing full-stack web apps with React & Node.js) hasn’t changed, there was significantly less pushback to the concept than before. I guess it comes down to the time elapsed and the product being more polished and validated from the outside - Beta, [published use-cases](/#showcases), [testimonials](/#testimonials), …

## Announcing a launch date publicly is great for productivity

Another big benefit we noticed from this type of launching is how much more productive it made the whole team. Although the launch date was totally self-imposed (and we did move it a couple of times internally), it was still an amazing forcing function once we announced it publicly. It focused the efforts of the whole team and it also felt great.

**We decided to keep going with the quarterly release schedule in this format - 3 months is just enough time to make a dent on the product side, but not long enough to get stuck or caught up with endless refactoring**. It also forces us to plan for the features that will have most impact on the developers using Wasp and make their lives easier, because we all want to have something cool and useful to present during the launch week.

## Conclusion

I hope you found this post helpful or at least interesting! Creating a new web framework might be one of the most notorious things to do as a developer, but that shouldn't be a reason not to do it - where are the new frameworks going to come from otherwise?

*Want to stay in the loop? → [Join our newsletter!](/#signup)*
