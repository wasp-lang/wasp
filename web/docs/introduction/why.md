---
title: Why Wasp?
---

**Wasp is a full-stack web framework**. Most frameworks cover only your frontend or only your backend. Wasp owns your whole app - client, server, and database - and understands how the parts fit together.

If you want to build a web app without the complexity of managing your stack, then Wasp is for you.

## Features

**Wasp is a batteries-included framework**. It includes everything you need to create a web app, built-in.

Some highlights include:

- **[Spec](/docs/general/spec)**: A single, high-level description of your app, in TypeScript. Describe your routes, auth, jobs, and data operations in `*.wasp.ts` file(s) and Wasp will maintain the full-stack code to match.
- **[Auth](/docs/auth/overview)**: Email, username, and social login (Google, GitHub, and more), with ready-made UI components and session handling. Fully customizable.
- **[Typesafe RPC](/docs/data-model/operations/overview)**: Call type-safe server functions straight from your React client. Wasp handles the API layer, data fetching, and cache invalidation for you.
- **[Async jobs](/docs/advanced/jobs)**: Run background tasks and recurring cron jobs, defined right in your spec. Powered by pg-boss, no dedicated infrastructure required.
- **[Email sending](/docs/advanced/email)**: Send email through providers like Resend, SendGrid and Mailgun.
- **[One-command deploy](/docs/deployment/deployment-methods/wasp-deploy/overview)**: `wasp deploy` ships your whole app - client, server, and database - to the host of your choice (Fly, Railway, and more coming).

## Design principles

**Wasp is an opinionated framework**. We came by these opinions over years of building for the web, studying what a web app really is, and imagining the ideal form to express it.

A few core principles guide how we build Wasp:

1. **Truly full-stack** - client, server, and database, in one framework.
2. **Managed experience over DIY** - we build the framework, you use it.
3. **Built like an onion** - start with the sane defaults, peel deeper when you need to.
4. **Greatest over latest** - a curated, cohesive stack, not the bleeding edge.
5. **Runs anywhere** - a standard React, Node.js and Postgres app, no lock-in.

### Truly full-stack

Full-stack isn't a feature we added later; it's the center of Wasp's design, and has been since day one. Most frameworks own your frontend or your backend and leave you to wire the two together - plus the database, auth, and deployment - yourself. Wasp owns all of it as one system.

Code generation is how we pull it off: Wasp compiles your Spec into the client, server, and database code, and keeps them in sync. That's also what makes full-stack type safety, one-command deploy, and features like auth and jobs work seamlessly across the whole stack.

### Managed experience over DIY

With most stacks, you assemble your framework yourself: pick a router, an ORM, an auth library, a job queue, and glue them together. With Wasp, you don't build your framework up, you build it down. You start with everything working out of the box - sane defaults, best practices, the boring parts handled - and take control only where it matters to you.

We maintain the stack so you can spend your time on your product, not on plumbing.

### Built like an onion

No single default fits every app, so nothing in Wasp is a dead end. Every layer is an API you can peel back. The top layer covers around 80% of cases with almost no code; when you need more, you drop down a layer and trade a little simplicity for more control.

You're never locked out of the details. You just don't have to deal with them until you want to.

### Greatest over latest

The web moves fast, and most of what's new won't matter in two years. We don't chase the bleeding edge. We track the ecosystem closely, then curate a small set of tools we trust - React, Node.js, Prisma - into one cohesive stack we stand behind.

You get modern, proven foundations without having to evaluate a new library every week.

### Runs anywhere

Wasp compiles to a standard React, Node.js, and Postgres app - nothing exotic, nothing proprietary. There's no Wasp runtime to depend on at runtime, and no infrastructure you're forced to rent.

Deploy your whole app in one command with `wasp deploy`, or take the generated code and host it yourself, anywhere. Your app is yours.
