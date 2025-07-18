---
title: "Why 3 SaaS Founders Chose Wasp As Their React & NodeJS Full-Stack Framework"
authors: [vinny]
image: /img/case-studies/saas-w-wasp.png
tags: [wasp, saas, indiehackers, boilerplate]
---

import Link from '@docusaurus/Link';
import useBaseUrl from '@docusaurus/useBaseUrl';
import ImgWithCaption from './components/ImgWithCaption'

Wasp is the only framework for React, NodeJS, and Prisma/Postgres with batteries-included that allows you to start building your app features right away. It does this by *intelligently* managing boilerplate code for you from a simple config file. On top of that, it also comes with a free SaaS starter kit to get you launching profitable apps in no time.

But don't take our word for it. Here are three founders, in their own words, that use [Wasp](https://wasp.sh/) and its free [Open SaaS template](https://opensaas.sh) to turn their ideas into successful SaaS apps, fast. 

---

## Case Study 1: Kivo - Democratizing Data Reporting

<iframe width="560" height="315" src="https://www.youtube.com/embed/R4xZIax9Gac?si=Qk-_OzCLocwai5yW" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen></iframe>

### The Challenge
Data analysis is messy. Dimitris, one of Kivo's co-founders, was tired of juggling countless tools (Excel for data, Python for analysis, and Word for reporting). He knew they could simplify the whole data analysis pipeline.

### The Solution
[Kivo](https://kivo.dev) is a beautiful, unified platform for creating data-driven reports. Think Notion, but with powerful, AI-assisted data editing capabilities built right in. You upload a file, Kivo helps you clean it, and you can generate full reports, charts, and more using prompt-based tools.

### How Wasp Helped
For Kivo, speed was everything. They needed to build a complex, data-intensive application with a small team.

-   **Rapid Development:** Wasp allowed the team to go **"from zero to one real fast"** while Open SaaS took care of the SaaS-specific boilerplate.
-   **Powerful Backend:** Wasp's integrated `APIs` and background `Jobs` made building their data processing and report generation pipeline 10x faster.
-   **The Founder's Take:** "The reason that Kivo is a reality is because of Wasp."

Kivo is an impressive app and proof that a small, focused team can build a sophisticated product when the framework handles the heavy lifting.

```ts
// This is how easy it is to add a custom API endpoint to your Wasp app. 😎

api fooBar { 
  fn: import { fooBar } from "@src/apis",
  httpRoute: (GET, "/foo/bar")
}
```

---

## Case Study 2: MicroInfluencer - Finding Niche Creators with Precision

<iframe width="560" height="315" src="https://www.youtube.com/embed/Oa88FJZGOPA?si=mT1i_Ofc1_JF3deq" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen></iframe>

### The Challenge
How do you find the *right* influencers? For marketers, finding and vetting a creator that will be a good fit for your product can be a massive cost.

### The Solution
[MicroInfluencer](https://microinfluencer.club/) is a smart analytics and research platform that helps brands discover and vet niche creators. It uses an AI-powered search to provide deep, data-driven statistics that reveal a creator's true value.

### How Wasp Helped
As a solo founder, Cam needed the agility to pivot and iterate quickly.

-   **Frictionless Pivoting:** Wasp's structure on top the [Open SaaS template](https://opensaas.sh) allowed him to quickly rebuild his SaaS from a marketplace into the powerful analytics and discovery tool it is today.
-   **The Perfect "Battery":** He calls Wasp's `Cron Jobs` his favorite feature and the backbone of the app. They run constantly to keep creator data fresh and even power an automated B2B lead generation system. He was able to add the AI search feature in just **one evening**.
-   **The Founder's Take:** "I think it is just the frictionless features like Cron Jobs. Just how quickly you can get something up and running that is genuinely so commercially useful."

MicroInfluencer shows how Wasp gives solo developers the space and leverage to build data-heavy, commercially valuable applications without a big team or budget, but just some free time and determination instead.

```ts
// This is how easy it is to add a cron job to your Wasp app. 😎

job mySpecialJob {
  executor: PgBoss,
  perform: {
    fn: import { foo } from "@src/workers/bar"
  },
  schedule: {
    cron: "0 * * * *",
  }
}
```

---

## Case Study 3: CTO Box - Empowering Engineering Leaders

<ImgWithCaption source="/img/case-studies/ctobox.png" alt="CTO Box" caption="CTO Box" />

### The Challenge
Most engineering managers lack purpose-built tools for crucial tasks like running effective 1-on-1s or building clear career ladders, often relying on messy spreadsheets and random templates.

### The Solution
[CTO Box](https://ctobox.sergiovisinoni.com/) is the toolkit that founder Sergio, a long-time engineering leader, always wished he had. It's a focused app that helps managers run better dev teams with structured 1-on-1s, AI-powered action items, and a dedicated career ladder builder.

### How Wasp Helped
Sergio wanted to build his solution without falling into the "hobby project curse" where endless boilerplate kills your motivation.

-   **Overcoming Boilerplate Fatigue:** The [Open SaaS template](https://opensaas.sh) was the **"killer feature"** for him. It provided the foundation (auth, DB, payments) he needed to get started immediately.
-   **Focus on What Matters:** Wasp let him focus on the important features and get feedback from real users to iterate on.
-   **The Founder's Take:** "It allowed me to just jump into the idea and forget about the boilerplate code."

CTO Box, which is still in private beta (but now accepting waitlist signups), proves that with the right foundation, you can finally build that tool you've always *needed* and get valuable feedback to bring it to market before you run out of steam.

---

## Your Turn to Build

The common thread here is simple: Wasp provides **momentum** and allows builders to **focus** on the **unique features** of their app. 

By handling the boilerplate, it empowers developers and small teams to build, launch, and iterate on complex applications faster than any other framework.

If you've been sitting on an idea, let these stories be your inspiration:

- Get started by [installing Wasp](https://wasp.sh/docs/quick-start)
- Then pull a fresh [Open SaaS template](https://opensaas.sh) with `wasp new -t saas`
- Check out the [Docs](https://docs.opensaas.sh) for more info