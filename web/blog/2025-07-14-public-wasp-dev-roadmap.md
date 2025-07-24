---
title: "Wasp now has a public development roadmap!"
authors: [martinsos]
image: /img/public-wasp-dev-roadmap/roadmap-screenshot-fire-cover.png
tags: [wasp]
---

import Link from '@docusaurus/Link';
import useBaseUrl from '@docusaurus/useBaseUrl';

import InBlogCta from './components/InBlogCta';
import WaspIntro from './_wasp-intro.md';
import ImgWithCaption from './components/ImgWithCaption'


All, behold the majestic [public dev roadmap of Wasp](https://github.com/orgs/wasp-lang/projects/5)!

<ImgWithCaption
    alt="Wasp dev roadmap (on fire)"
    caption="still hot"
    source="img/public-wasp-dev-roadmap/roadmap-screenshot-fire.png"
/>

Read on to learn about the **Why**, **How**, and **What** of the roadmap.

## *Why* the public dev roadmap

So far, this is how we've been doing our planning at Wasp:
- Each quarter, **plan quarterly objectives** for the next 3 months, with the big vision in mind.
- Each sprint, **plan sprint tasks** for the next 2 weeks, with the quarterly objectives in mind.

<ImgWithCaption
    alt="Diagram of how we did planning before"
    caption="How the Wasp team of the ancients did planning"
    source="img/public-wasp-dev-roadmap/old-planning-diagram.webp"
/>

The tricky part was, that while we always had a big plan and vision in our minds, it was always somewhat implicit, an oral tradition among the team members. There'd be multiple, quite similar but different "big planning" pages on Notion with ideas and prioritized features that would quickly become outdated, and new Notion pages would replace them.

And it made sense: **Wasp was changing fast**, we were ideating, experimenting, and the team was small enough.

But, that changed: **our team grew to 8 people (and will grow more)**, ideas stabilized, Wasp itself grew more complex, people building with Wasp (aka Waspeteers) started asking more often when is which feature coming, and finally, we put our focus on the 1.0 release of Wasp and planning for it.

So that's why we're introducing our public development roadmap!

Firstly, it makes our plans transparent, allowing everybody to know what to expect for Wasp's future and what they are betting on when investing their time into it, and secondly, it also serves as a central, living place for our "big" plan. It also shifts our quarterly planning from "big replanning" to just adjusting the roadmap.

## The *How* of the roadmap

Check the [Wasp repo](https://github.com/wasp-lang/wasp) and you'll see that we track all our ideas as GitHub issues.

Existing feature improvements, crazy experimental ideas, new big features, documentation improvements, discussions, stuff that annoys us, bugs, …. If you wonder about how we plan to support something in Wasp in the future, there is a fair chance you will find the GitHub issue for it in our repo.

So, where did we decide to put the roadmap tasks? Well, in GitHub issues, of course!

<ImgWithCaption
    alt="Meme with astronauts about how it was always all github issues at Wasp"
    caption="and will always be"
    source="img/public-wasp-dev-roadmap/it-is-all-gh-issues.webp"
/>

We decided to implement the roadmap as a GitHub project that is a collection of epics (GitHub issues with "issue type" of "Epic").

No regular issues are allowed in it, only epics, and there can't be too many, a couple of dozen. These epics then have subtasks which are normal Github issues.

Therefore, the roadmap serves as a high-level overview of how we plan to execute on all those GitHub issues of ours that we already had.

## *What* is in the roadmap: Wasp 1.0, AI, wild ideas & more

The big motivation for creating the roadmap was planning for the **1.0 release of Wasp**: we felt like we knew enough at this point to make a detailed plan of everything that should be a part of the 1.0 release; therefore, most of the epics are labelled with the `1.0` milestone.

We also added a couple of epics that are still **in the idea stage** but that get mentioned/requested regularly or that we are excited about, like [access control ((RBAC/ABAC)](https://github.com/wasp-lang/wasp/issues/2896), [modular database](https://github.com/wasp-lang/wasp/issues/2893) (imagine choosing between Drizzle and Prisma), [Wasp Studio](https://github.com/wasp-lang/wasp/issues/2892), ….

It’s hard to ignore AI these days, so you will also find [the epic for ensuring the best experience in Wasp when using it with vibe/AI-assisted coding](https://github.com/wasp-lang/wasp/issues/2630): starter Cursor/Claude rules for a fresh Wasp project, llms.txt, Wasp MCP, ….

The dominant bulk of the roadmap is 1.0-focused epics, though, and we can roughly divide them into a couple of big categories:
- ✅ **The essentials**
- 🚀 **Main features**
- 🧪 **Experimental features**

<ImgWithCaption
    alt="Diagram of the composition of Wasp dev roadmap (circles)."
    source="img/public-wasp-dev-roadmap/wasp-roadmap-circle-diagram.webp"
/>

### ✅ The essentials - getting it right

The focus here is on getting the core parts of Wasp, both internal and external, in order and working great for everybody using it and for us while developing it.

While developing pre-1.0 Wasp, a big part of what we do is trying out new concepts and ideas and making sure they work well together (e.g., DSL, code generation, integrated Auth, TS spec, query invalidation based on entities, …).

Since this means that we often do things in a non-standard way for a typical web framework (that is kind of our thing), it also means that we sometimes get non-standard rough edges as a result.

Once those accumulate somewhat and we learn more about them, it also becomes clear how to solve them, and that is a big part of what we are doing here.

To give you an idea, some big epics here are:

- [**Making Wasp "right"**](https://github.com/wasp-lang/wasp/issues/2870): polishing rough edges regarding the IDE support, standard config files for JS projects, how the generated Wasp project is structured internally (e.g., start using npm workspaces), …. A lot of stuff you don’t see if it works as it should, but will notice if it is not.
- [**Polishing basic DX**](https://github.com/wasp-lang/wasp/issues/2884): with time, we learned a lot about all the little bumps on the road to using Wasp, and while we try to fix them regularly, we often prioritized working on bigger features. Now is a good time to take a look at all those together and do a big clean-up!

### 🚀 Main features - leveling up!

The main features of Wasp (Operations, Entities, Jobs, Auth, …) have been stable for some time, and we learned a ton about them in the recent period, both based on our usage and also based on the feedback and questions from all the awesome people in our community (Discord, Github) that are using Wasp.

For each of the features, we now have a clearer plan of where we want that feature to be for 1.0; therefore, most of these epics are about taking the existing features to the next level.

This means expanding them (e.g. adding account merging to [Auth](https://github.com/wasp-lang/wasp/issues/2875)), fixing rough edges (e.g. more flexibility when using [web sockets](https://github.com/wasp-lang/wasp/issues/2882)), and also redesigning and rethinking some central parts (e.g. [Operations](https://github.com/wasp-lang/wasp/issues/2876), [Custom API](https://github.com/wasp-lang/wasp/issues/2880), and their relationship) while keeping what you all like the best about them and doubling down on improving the parts that could be better.

### 🧪 Experimental features - Full Stack Modules, Wasp Studio, …

We have some ideas that we have been mulling over for a long time, always considering them, but despite our excitement, we haven't found the time to prioritize them so far.

Some of those that we feel are both ready and important enough for shaping up Wasp into 1.0, we added to the roadmap!

A big one is [**FSMs (Full Stack Modules)**](https://github.com/wasp-lang/wasp/issues/2873), a concept we have been discussing for years now: "Oh yeah, this will likely be solved by FSMs", or "We could just extract this part into an FSM once we have them".  
FSMs will be a way to define pieces of Wasp apps (e.g., file uploading, Stripe integration, or AI chat) in a modular and distributable way, spanning the whole stack, from frontend to the database (Imagine Ruby on Rails Engines, but in Wasp style)! Libraries/packages, but truly full-stack. Will it work? We don’t know yet, but if it does, it could be awesome!

## *Where* next

We are inviting you to follow the roadmap, share feedback, and contribute!

<ImgWithCaption
    alt="Two dabois at the campfire, chilling."
    caption="You made it till the end! Join us at the Wasp campfire and relax."
    source="img/public-wasp-dev-roadmap/wasp-daboi-campfire.png"
/>

Let us know if there is something that you think should be on the roadmap but is missing, if you would do something different, or if you think one of the epics we picked for the roadmap is a great choice and we should double down on it.

You can comment directly on our [GitHub project](https://github.com/orgs/wasp-lang/projects/5)/issues, or in our [Discord](https://discord.gg/rzdnErX) where the whole Wasp team is very active.

If a specific epic particularly interests you, you can subscribe to it easily: click on “Subscribe” at the bottom of the sidebar on the right of the GitHub page for that issue.

Also, let us know if there is some other part of Wasp besides the roadmap that you would like to learn more about and get more transparency into!

In the future, we plan to regularly update the roadmap as we work on epics, refine them, and release them, and also on a quarterly basis or on demand as we possibly replan parts of the roadmap.

p.s. You will notice there was no mention of [Open SaaS](https://opensaas.sh/), our flagship Wasp starter template, in the roadmap; that is because Open SaaS has a development cycle of its own. You can find all the plans for OpenSaas in its Github repo, though (e.g., there are plans for adding more payment providers soon), and we will, as always, also be updating it with every new version of Wasp so it uses the latest and greatest (imagine Open SaaS using [FSMs](https://github.com/wasp-lang/wasp/issues/2873) → woah!).
