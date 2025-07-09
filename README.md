<p align=center>
  <img height="80px" src="https://user-images.githubusercontent.com/1536647/77317442-78625700-6d0b-11ea-9822-0fb21e557e87.png"/>
</p>
<p align=center>
  The fastest way to develop full-stack web apps with React & Node.js.
</p>
<br>
<p align=center>
  <a href="https://github.com/wasp-lang/wasp/blob/main/LICENSE"><img alt="license" src="https://img.shields.io/github/license/wasp-lang/wasp"></a>
  <a href="https://github.com/wasp-lang/wasp/releases/latest"><img alt="latest release" src="https://img.shields.io/github/v/release/wasp-lang/wasp"/></a>
  <a href="https://discord.gg/rzdnErX"><img alt="discord" src="https://img.shields.io/discord/686873244791210014?label=chat%20@%20discord"/></a>
</p>

---

<p align="center">
  <a href="https://wasp.sh">Web page</a> | <a href="https://wasp.sh/docs">Docs</a> | <a href="https://e44cy1h4s0q.typeform.com/to/EPJCwsMi">Deployed? Get swag! 👕</a>
</p>

<br>

Wasp (**W**eb **A**pplication **Sp**ecification) is a Rails-like framework for React, Node.js, and Prisma.  
Build your app in a day and deploy it with a single CLI command!

### Why is Wasp awesome

- 🚀 **Quick start**: Due to its expressiveness, you can create and deploy a production-ready web app from scratch with very few lines of concise, consistent, declarative code.
- 😌 **No boilerplate**: By abstracting away complex full-stack features, there is less boilerplate code. That means less code to maintain and understand! It also means easier upgrades.
- 🔓 **No lock-in**: You can deploy the Wasp app anywhere you like. There is no lock-in into specific providers; you have full control over the code (and can actually check it out in .wasp/ dir if you are interested ).

### Features

🔒 Full-stack Auth, 🖇️ RPC (Client <-> Server), 🚀 Simple Deployment, ⚙ ️Jobs, ✉️ Email Sending, 🛟 Full-stack Type Safety, ...

### Code example

Simple Wasp config file in which you describe the high-level details of your web app:

```js
// file: main.wasp

app todoApp {
  title: "ToDo App",  // visible in the browser tab
  wasp: { version: "^0.15.0" },
  auth: { // full-stack auth out-of-the-box
    userEntity: User, methods: { email: {...} }
  }
}

route RootRoute { path: "/", to: MainPage }
page MainPage {
  authRequired: true, // Limit access to logged-in users.
  component: import Main from "@client/Main.tsx" // Your React code.
}

query getTasks {
  fn: import { getTasks } from "@server/tasks.js", // Your Node.js code.
  entities: [Task] // Automatic cache invalidation.
}

// file: schema.prisma

model Task { // Your Prisma data model.
  id          Int     @id @default(autoincrement())
  description String
  isDone      Boolean @default(false)
}
```

The rest of the code you write in React / Node.js / Prisma and just reference it from the .wasp file.

:arrow_forward: Check out [TodoApp example](/examples/tutorials/TodoApp) for the complete code example. :arrow_backward:

### How it works

<img width="800px" src="https://user-images.githubusercontent.com/1536647/231472285-126679e5-ecce-4cbb-8579-eb3cd9ba95bf.png"/>

Given a simple .wasp configuration file that describes the high-level details of your web app, and .js(x)/.css/..., source files with your unique logic, Wasp compiler generates the full source of your web app in the target stack: front-end, back-end and deployment.

This unique approach is what makes Wasp "smart" and gives it its superpowers!

For more information about Wasp, check [**docs**](https://wasp.sh/docs).

# Have a Wasp app deployed? - we will send you swag!

If you have a Wasp application running in production, we'd love to send some swag your way! Fill out [this form](https://e44cy1h4s0q.typeform.com/to/EPJCwsMi), and we'll make it happen.

# Get started

Run

```
curl -sSL https://get.wasp.sh/installer.sh | sh
```

to install Wasp on OSX/Linux/WSL(Win). From there, just follow the instructions to run your first app in less than a minute!

For more details, check out [the docs](https://wasp.sh/docs).

# Wasp AI / Mage

Wasp comes with experimental AI code generator to help you kickstart your next Wasp project -> you can use it via `wasp new` in the CLI (choose "AI" option) if you can provide your OpenAI keys or you can do it via our [Mage web app](https://usemage.ai) in which case our OpenAI keys are used in the background.

# This repository

This is the main repo of the Wasp universe, containing core code (mostly `waspc` - Wasp compiler) and the supporting materials.

# Project status

Currently, Wasp is in beta, with most features fleshed out and working well.
However, there are still a lot of improvements and additions that we have in mind for the future, and we are working on them constantly, so you can expect a lot of changes and improvements in the future.

While the idea is to support multiple web tech stacks in the future, right now, we are focusing on the specific stack: React + react-query, NodeJS + ExpressJS, and Prisma.

# Contributing

Any way you want to contribute is a good way :)!

The best place to start is to check out [waspc/](waspc/), where you can find detailed steps for first-time contributors + technical details about the Wasp compiler.

The core of Wasp is built in Haskell, but there are also a lot of non-Haskell parts of Wasp, so you will certainly be able to find something for you!

Even if you don't plan to submit any code, just joining the discussion on discord [![Discord](https://img.shields.io/discord/686873244791210014?label=chat%20on%20discord)](https://discord.gg/rzdnErX) and giving your feedback is already great and helps a lot (motivates us and helps us figure out how to shape Wasp)!

You can also:

- :star: Star this repo to show your interest/support.
- :mailbox: Stay updated by subscribing to our [email list](https://wasp.sh#signup).
- :speech_balloon: Join the discussion at https://github.com/wasp-lang/wasp/discussions .
- :eyes: Check out the [development roadmap](https://github.com/orgs/wasp-lang/projects/5).

# Careers

Check our [careers](https://wasp-lang.notion.site/Wasp-Careers-59fd1682c80d446f92be5fa65cc17672) page for a list of currently opened positions!

# Sponsors

<a href="https://github.com/michelwaechter"><img src="https://github.com/michelwaechter.png" width="50px" alt="michelwaechter" /></a> - Our first sponsor ever! Thanks so much, Michel ❤️ , from the whole Wasp Team, for bravely going where nobody has been before :)!

<a href="https://github.com/shayneczyzewski"><img src="https://github.com/shayneczyzewski.png" width="50px" alt="shayneczyzewski" /></a> - Thanks Shayne, for all the contributions you did so far and for your continuous support!

<a href="https://github.com/velocity-one"><img src="https://github.com/velocity-one.png" width="50px" alt="VelocityOne" /></a> - Thanks VelocityOne for the generous donation!

<a href="https://github.com/ricdex"><img src="https://github.com/ricdex.png" width="50px" alt="ricdex" /></a> - We are thankful for your support Ricardo in this early stage of Wasp :)!

<a href="https://github.com/ThomasJonesUK"><img src="https://github.com/ThomasJonesUK.png" width="50px" alt="ThomasJonesUK" /></a> - Thanks Thomas for supporting Wasp :)!

<a href="https://github.com/Arukaito"><img src="https://github.com/Arukaito.png" width="50px" alt="Arukaito" /></a> - Our awesome sponsor again and again!

<a href="https://github.com/Case-E"><img src="https://github.com/Case-E.png" width="50px" alt="Case-E" /></a> - Big thanks for supporting us both via sponsorship and great suggestions!

## Past sponsors

<a href="https://github.com/MarianoMiguel"><img src="https://github.com/MarianoMiguel.png" width="50px" alt="MarianoMiguel" /></a>
<a href="https://github.com/Tech4Money"><img src="https://github.com/Tech4Money.png" width="50px" alt="Tech4Money" /></a>
<a href="https://github.com/haseeb-heaven"><img src="https://github.com/haseeb-heaven.png" width="50px" alt="haseeb-heaven" /></a>
