---
title: 'Why we chose Prisma as a database layer for Wasp'
authors: [martinsos]
image: /img/why-we-chose-prisma/wasp-loves-prisma.png
tags: [webdev, wasp, prisma]
---

import Link from '@docusaurus/Link';
import useBaseUrl from '@docusaurus/useBaseUrl';

import InBlogCta from './components/InBlogCta';
import WaspIntro from './_wasp-intro.md';
import ImgWithCaption from './components/ImgWithCaption'

<ImgWithCaption
    alt="Beta is coming"
    source="img/why-we-chose-prisma/wasp-loves-prisma.png"
/>

Wasp is a full-stack JS web dev framework, covering frontend, backend, and database. When choosing the solution to build our database layer on top, we chose Prisma, even though it was still somehwat new tech at that point, and we believe today we made a great choice -> read on to learn why!

<!--truncate-->

At Wasp, [we aim](/docs/vision) to simplify full-stack web development via a specialized high-level language. This language allows you to describe the main parts of your web app succinctly, avoiding a lot of usual boilerplate and configuration while giving you lots of features and ensuring best practices. Wasp is essentially a full-stack web framework implemented as a specialized language that works with React & Node.js!

When we started working on Wasp, we wanted to keep it easy to learn and to the point, so we decided:

- the Wasp language should only be used at a high level, so you would still use React, NodeJS, HTML, CSS, etc. to implement your custom logic. If a full-stack web app is an orchestra, Wasp is the conductor.
- the Wasp language should be declarative and simple, very similar to JSON, but “smarter” in the sense it understands web app concepts and makes sure your app follows them.

With that in mind, we focused on identifying high-level web app concepts that are worth capturing in the Wasp language. We identified the following parts of a web app:

- General app info (title, head, favicon, …)
- Pages and Routes
- Data Models (aka Entities), e.g. User, Task, Organization, Article, … .
- Operations (communication between client and server; CRUD on data models, 3rd party APIs, …)
- Deployment

## Entities

Of all of those, Entities are in the middle of everything, present through the whole codebase, and are central to all the other parts of the web app: client, server, and database. They were, however, also the most daunting part to implement!

When we started, we imagined an Entity would look something like this in Wasp:

```
entity User {
  id: Id,
  username: String @unique,
  email: String @unique
  groups: [Group]
}
```

While adding this initial syntax to our language was feasible, there were also much bigger tasks to tackle in order to make this a proper solution:

- expand syntax to be flexible enough for real-life use cases
- support migrations (data and schema)
- generate code that users can call from JS/TS to query and update entities in the DB
- and probably a lot of other things that we hadn’t even thought of yet!

## Mongoose, Sequelize, … or Prisma?

We already decided that we would pick an ORM(ish) solution for JS/TS which we would build the rest of the features on top of. We started evaluating different ones: Mongoose, Sequelize, TypeORM, … .

But then we looked at Prisma, and the winner was clear! Not only was Prisma taking care of everything that we cared about, but it had one additional feature that made it a perfect fit:

```
model User {
  id          Int     @id @default(autoincrement())
  username    String  @unique
  password    String
}
```

No, this is not another idea of how the syntax for Entities could look like in Wasp language → this is the Prisma Schema Language (PSL)!!!

## Prisma Schema Language (PSL)

Indeed, Prisma is unique in having a special, declarative language for describing data models (schema), and it was exactly what we needed for Wasp.

So instead of implementing our own syntax for describing Entities, we decided to use Prisma and their PSL to describe Entities (data models) inside the Wasp language.

Today, Entities are described like this in Wasp language:

```
... some Wasp code ...

entity User {=psl
  id          Int     @id @default(autoincrement())
  username    String  @unique
  password    String
psl=} 

... some Wasp code ...
```

So in the middle of Wasp, you just switch to writing PSL (Prisma Schema Language) to describe an entity!

Another great thing is that the PSL is at its core a pretty simple language, so we [implemented our own parser](https://github.com/wasp-lang/wasp/blob/main/waspc/src/Wasp/Psl/Parser/Model.hs) for it → that means that Wasp actually understands what you wrote, even though it is PSL, and can fully work with it. So we lost nothing by using PSL instead of our own syntax and instead gained all the features that Prisma brings.

## Other Benefits

Besides PSL, there were plenty of other reasons why we felt Prisma is a great fit for us:

 -  It is targeting Javascript / Typescript.
 -  It takes care of migrations and has a nice workflow for doing it.
 -  It supports different databases: Mongo, PostgreSQL, CockroachDB, …, which is very important for Wasp since our vision is to support different stacks in the future.
 -  It has Prisma Studio - UI for inspecting your database, which we also make available to you via Wasp CLI.
 -  It keeps improving quickly and is very focused on a nice developer experience, which is also our focus here at Wasp.
 -  Community is extremely welcoming and the core team is super helpful - all of our questions and issues were answered super quickly!

## Challenges

While integrating Prisma into Wasp went really smoothly, there were a few hiccups:

 -  Getting Prisma CLI to provide interactive output while being called programmatically by Wasp was tricky, and in the end, we had to use a bit of a dirty approach to trick the Prisma CLI into thinking it is called interactively. We opened an issue for this with Prisma, so hopefully, we will be able to remove this once it is resolved: https://github.com/prisma/prisma/issues/7113.
 -  In the early days, there were some bugs, however, they were always quickly solved, so updating to the newest Prisma version was often the solution.
 -  It took us a bit of fiddling to get Prisma to work with its schema outside of the server’s root directory, but we did get it working in the end!

Most of these were due to us stretching the boundaries of how Prisma was imagined to be used, but in total Prisma proved to be fairly flexible!

## Summary

With its declarative language for describing schema, focus on ergonomics, and JS/TS as the target language, Prisma was really a stroke of luck for us - if not for it, it would have taken much more effort to get the Entities working in Wasp.

When we started using it, Prisma was still somewhat early, and it was certainly the least-mature technology in our stack - but we decided to bet on it because it was just a perfect fit, and it made so much sense. Today, with Prisma being a mature and popular solution, we are more than happy we made that choice!

## Future

Already, Prisma is playing a big role at Wasp, but there is still more that we plan and want to do:

 - support Prisma’s Enum and Type declarations
 - expose more of Prisma’s CLI commands, especially database seeding
 - add support in Wasp for multiple databases (which Prisma already supports)
 - improve IDE support for PSL within the Wasp language

If you are interested in helping with any of these, reach out to us on this issue https://github.com/wasp-lang/wasp/issues/641, or in any case, join us on our [Discord server](https://discord.gg/rzdnErX)!
