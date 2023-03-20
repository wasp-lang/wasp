<p align=center>
  <img height="80px" src="https://user-images.githubusercontent.com/1536647/77317442-78625700-6d0b-11ea-9822-0fb21e557e87.png"/>
</p>
<p align=center>
  The fastest way to develop full-stack web apps with React & Node.js.
</p>
<br>
<p align=center>
  <a href="https://github.com/wasp-lang/wasp/blob/main/LICENSE"><img alt="license" src="https://img.shields.io/github/license/wasp-lang/wasp"></a>
  <a href="https://github.com/wasp-lang/wasp/search?l=haskell"><img alt="language" src="https://img.shields.io/badge/language-Haskell-purple.svg"></a>
  <a href="https://github.com/wasp-lang/wasp/actions"><img alt="build status" src="https://img.shields.io/github/workflow/status/wasp-lang/wasp/CI"/></a>
  <a href="https://discord.gg/rzdnErX"><img alt="discord" src="https://img.shields.io/discord/686873244791210014?label=chat%20@%20discord"/></a>
</p>

------

- [**Project page**](https://wasp-lang.dev)
- [**Docs**](https://wasp-lang.dev/docs)

<br>

Develop full stack web apps without boilerplate. Describe common features via Wasp DSL and write the rest in React, Node.js and Prisma.

Wasp (**W**eb **A**pplication **Sp**ecification Language) is a declarative configuration tool for developing, building and deploying modern full stack web apps with less code.

```js
// file: main.wasp

app TodoApp {
  title: "Todo App",
  wasp: { version: "^0.7.0" }
}

route RootRoute { path: "/", to: MainPage }
page MainPage {
  component: import Main from "@ext/pages/Main.jsx" // Importing React component.
}

query getTasks {
  fn: import { getTasks } from "@ext/queries.js", // Importing NodeJS code.
  entities: [Task]
}

entity Task {=psl
    id          Int     @id @default(autoincrement())
    description String
    isDone      Boolean @default(false)
psl=}
```

Source files (`.wasp`, `.js(x)`, `.css`, ...) are compiled by `waspc` (Wasp compiler) into the web technology stack of your choice (currently React + NodeJS/ExpressJS + Prisma).

:arrow_forward: Check out [TodoApp example](/examples/tutorials/TodoApp) for the complete code example. :arrow_backward:

Why is Wasp awesome:
- **Quick start**: Due to its expressiveness, you can create and deploy a production-ready web app from scratch with very few lines of concise, consistent, declarative code.
- **Flexible**: When you need more control than Wasp offers, you can write code in existing technologies such as js/html/css/... and combine it with Wasp code!
- **No lock-in**: If Wasp becomes too limiting for you, simply eject and continue with the generated source code, which is human-readable.

For more information about Wasp, check [**docs**](https://wasp-lang.dev/docs).

# Get started

Run
```
curl -sSL https://get.wasp-lang.dev/installer.sh | sh
```
to install Wasp on OSX/Linux. From there, just follow the instructions to run your first app in less than a minute!

For more details (including installing on Windows) check out [the docs](https://wasp-lang.dev/docs).

# This repository

This is the main repo of the Wasp universe, containing core code (mostly `waspc` - Wasp compiler) and the supporting materials.

# Project status

Currently, Wasp is in beta, with most features flushed out and working well.
However, there are still a lot of improvements and additions that we have in mind for the future, and we are working on them constantly, so you can expect a lot of changes and improvements in the future.
As Wasp grows further, it should allow the development of web apps of increasing complexity!

While the idea is to support multiple web tech stacks in the future, right now we are focusing on the specific stack: React + react-query, NodeJS + ExpressJS, Prisma. We might yet change that as time goes on, taking trends into account, but for now, this is serving us well to develop compiler and language.

# Contributing

Any way you want to contribute is a good way :)!

The best place to start is to check out [waspc/](waspc/), where you can find detailed steps for the first time contributors + technical details about the Wasp compiler.

Even if you don't plan to submit any code, just joining the discussion on discord [![Discord](https://img.shields.io/discord/686873244791210014?label=chat%20on%20discord)](https://discord.gg/rzdnErX) and giving your feedback is already great and helps a lot (motivates us and helps us figure out how to shape Wasp)!

You can also:
 - :star: Star this repo to show your interest/support.
 - :mailbox: Stay updated by subscribing to our [email list](https://wasp-lang.dev#signup).
 - :speech_balloon: Join the discussion at https://github.com/wasp-lang/wasp/discussions .

# Careers

Check our [careers](https://wasp-lang.notion.site/Wasp-Careers-59fd1682c80d446f92be5fa65cc17672) page for a list of currently opened positions!
