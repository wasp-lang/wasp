---
title: Why Wasp?
---

**Wasp is a full-stack framework for building web apps you can understand.**

A web app consists of a frontend, a backend, and a database, along with the libraries that connect them: a router, an ORM, an authentication library, a job queue, and so on.

In most other frameworks, it is your job to assemble all these pieces, and then keep them working together. There is no full picture of what your app actually does - it is scattered throughout your code, config files, and folder conventions.

That is the whole idea behind Wasp - it gives you that picture. Everything else follows from it.

## A description you can read - your app's Spec

Wasp starts from one idea: **your app should have a description you can read and come away understanding what the app does.**

You write that description in TypeScript, in `.wasp.ts` files - we call this layer [the Spec](/docs/general/spec). You can think of it as a schema for your app, the way your database has one for your data.

```ts title="main.wasp.ts"
import { action, app, job, page, query, route } from "@wasp.sh/spec";
// Cron jobs
import { sendDailyDigest } from "./src/jobs" with { type: "ref" };
// React components
import { LoginPage } from "./src/LoginPage" with { type: "ref" };
import { MainPage } from "./src/MainPage" with { type: "ref" };
// Data operations (server functions)
import { getTasks } from "./src/queries" with { type: "ref" };
import { createTask } from "./src/actions" with { type: "ref" };

export default app({
  name: "TodoApp",
  wasp: { version: "{latestWaspVersion}" },
  title: "TodoApp",
  auth: {
    userEntity: "User",
    methods: { usernameAndPassword: {} },
    onAuthFailedRedirectTo: "/login",
  },
  spec: [
    route("RootRoute", "/", page(MainPage, { authRequired: true })),
    route("LoginRoute", "/login", page(LoginPage)),

    query(getTasks, { entities: ["Task"] }),
    action(createTask, { entities: ["Task"] }),

    job(sendDailyDigest, { schedule: { cron: "0 9 * * *" } }),
  ],
});
```

That is the whole app: the pages, who can open them, the data operations, and what runs on a schedule. It says nothing about which router, session store, or cron library is used. Those are implementation details. You can go look at them when you need to, and reach past them when the defaults do not fit: add your own API endpoints, plug in Express middleware, write database queries by hand.

The code around this file is ordinary TypeScript. Your React components, your server functions, and your Prisma models are files in your project that you write and edit. Wasp generates the parts in between: the API layer between client and server, session handling, the job runner, and the deployment setup.

Because Wasp compiles the app from this file, it cannot fall out of date. The routes, auth, and jobs it lists are the ones the app actually has.

## What's in the box {#whats-in-the-box}

**Wasp comes with everything a web app needs.** Because Wasp knows about the whole app, these parts come already wired into the framework instead of being assembled by you from separate services and libraries.

- **[Auth](/docs/auth/overview)**: Email, username, and social login (Google, GitHub, and more), with ready-made UI components and session handling. Fully customizable.
- **[Typesafe RPC](/docs/data-model/operations/overview)**: Call type-safe server functions straight from your React client. Wasp handles the API layer, data fetching, and cache invalidation for you, powered by TanStack Query.
- **[Async jobs](/docs/advanced/jobs)**: Run background tasks and recurring cron jobs, defined right in your spec. Powered by pg-boss, no dedicated infrastructure required.
- **[Email sending](/docs/advanced/email)**: Send email through providers like Resend, SendGrid and Mailgun, or your own SMTP server.
- **[One-command deploy](/docs/deployment/deployment-methods/wasp-deploy/overview)**: `wasp deploy` ships your whole app - client, server, and database - to the host of your choice (Fly, Railway, and more coming), or [self-host it](/docs/deployment/deployment-methods/self-hosted) anywhere you can run Docker.

Each one is a few lines in the same file, not another system to set up and keep in sync.

## Why we built it this way {#why-we-built-it-this-way}

Web development moves fast. Tools, libraries, and frameworks come and go. Most frameworks are anchored to a specific moment in time: a UI library, a bundler, a way of deploying. That helps them early on, but it also means they age with the tools they were built around.

We wanted to anchor Wasp in something slower. Not the tools of the year, but what a web app actually is: pages, data, operations, auth, jobs, deployment. That list has barely changed in twenty years. What keeps changing is how we implement it.

So we spent years on one question: what is a web app, really? The answer became a way to describe one. Wasp stands for Web App Specification: a description of the app, which the compiler turns into a working React, Node.js, and Postgres app.

We also believe a web app is not a pile of parts. Most of what is worth knowing about it lives in the relationships between them: which types cross the client and server boundary, who is allowed to call which operation, what has to happen at deploy time. Each library in a hand-assembled stack sees only its own half, so none of them can act on any of that. A framework that knows about the frontend, the backend, and the database at once can do things a connector between libraries cannot. That knowledge is also what lets Wasp put the whole app in one readable file.

**Understanding was the goal from the start.** An app that is only assembled from parts has no description of itself anywhere: no plan, no blueprint, just the finished result, which you have to reverse engineer every time you want to know what it does. We wanted that blueprint to be a real file in your project, and to stay accurate as the app changes.

## What we believe {#what-we-believe}

**Wasp is an opinionated framework.** The opinions come from years of building web apps and working out what all of them have in common.

1. **Truly full-stack** - one framework knows about the frontend, the backend, and the database at once, so it can handle what happens between them: moving data across, keeping the types in sync, checking who is logged in. A stack you assemble yourself can only connect the parts.
2. **Managed experience over DIY** - the router, the ORM, auth and the job queue arrive already fitted together and working. Our job is that the whole thing feels like one product, whichever pieces you end up using.
3. **No dead ends** - when an abstraction stops fitting, you can shed it and go a layer deeper: [build your own auth UI](/docs/auth/username-and-pass/create-your-own-ui), add [your own API endpoints](/docs/advanced/apis), or write Prisma queries by hand.
4. **Greatest over latest** - we curate a reasonable, stable set of tools (React, Node.js, Prisma) and maintain the glue between them, instead of chasing the newest libraries.
5. **Runs anywhere** - Wasp compiles to a standard React, Node.js and Postgres app. Deploy it with `wasp deploy` or host the generated code yourself. No Wasp runtime, no lock-in.

## Where we're going {#where-we-are-going}

Today, Wasp is a batteries-included full-stack framework for JavaScript. That part is real, and you can ship with it now.

The bigger bet is what having a high-level description, a Spec, makes possible next. Swapping what is underneath without rewriting what is above. Installing full-stack features like payments or an admin panel as packages. Catching mistakes at compile time across the frontend and the backend at once. Every one of those needs a framework that is aware of all the parts, and we think most of what that makes possible is still ahead of us.

And as more code gets written by agents, the gap between what your app does and what you know about it keeps growing. A description is the best way we know to keep that gap small: you read the file, your agent writes code against it, and you can still tell what you shipped.

We are not all the way there yet. That is where we are heading, and we would love to have you along for the ride.

**Build web apps you can understand.**
