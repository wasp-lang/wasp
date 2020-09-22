<p align=center>
  <img height="80px" src="https://user-images.githubusercontent.com/1536647/77317442-78625700-6d0b-11ea-9822-0fb21e557e87.png"/>
</p>

<p align=center> A programming language that understands what a web app is. </p>
<br>

[![Discord](https://img.shields.io/discord/686873244791210014?label=chat%20on%20discord)](https://discord.gg/rzdnErX)
[![Build Status](https://travis-ci.com/wasp-lang/wasp.svg?branch=master)](https://travis-ci.com/wasp-lang/wasp)
[![Build Status](https://ci.appveyor.com/api/projects/status/github/wasp-lang/wasp?branch=master&svg=true)](https://ci.appveyor.com/project/Martinsos/wasp/branch/master)

- [**Project page**](https://wasp-lang.dev)
- [**Demo**](https://wasp-lang.dev/#demo)
- [**Docs**](https://wasp-lang.github.io/web/docs)

<br>

Wasp (**W**eb **A**pplication **Sp**ecification Language) is an extensible [DSL](https://en.wikipedia.org/wiki/Domain-specific_language) (domain-specific language) for building modern web apps with less code.

Concepts such as *app*, *page*, *user*, *login*, *frontend*, *production*, etc. are baked into the language, bringing a new level of expressiveness and allowing you to get more work done with fewer lines of code.

NOTE: Wasp is still in alpha, meaning it has bugs and many critical featuers are still missing and it is stil changing a lot!

```js
// todoApp.wasp:

app TodoApp {
  title: "ToDo app",
  favicon: "./todo-logo.png"
}

route "/" -> page Main
page Main {
    component: import Main from "@ext/pages/Main"
}

query getTasks {
  fn: import { getTasks } from "@ext/queries.js"
}

entityPSL Task {=psl
    id          Int     @id @default(autoincrement())
    description String
    isDone      Boolean @default(false)
psl=}
```

Source files (`.wasp`, `.js(x)`, `.css`, ...) are compiled (transpiled) by `waspc` (Wasp compiler) into the web technology stack of your choice (e.g. React + Redux + ExpressJS + ...).

:arrow_forward: Check out [TodoApp example](examples/todoApp) for complete code example. :arrow_backward:

Why is Wasp awesome:
- **Quick start**: Due to its expressiveness, you can create and deploy a production-ready web app from scratch with very few lines of concise, consistent, declarative code.
- **Flexible**: When you need more control than Wasp offers, you can write code in existing technologies such as js/html/css/... and combine it with Wasp code!
- **No lock-in**: If Wasp becomes too limiting for you, simply eject and continue with the generated source code, which is human-readable.

For more information about Wasp, check [**docs**](https://wasp-lang.github.io/web/docs).

# Get started
Check out [the docs](https://wasp-lang.github.io/web/docs/language/getting-started).

# This repository

This is the main repo of the Wasp universe, containing core code (mostly `waspc` - Wasp compiler) and the supporting materials.

# Project status

We are still very early (pre-alpha). Specifically, we are developing `waspc` (Wasp compiler) and also designing language as we go.

Currently, `waspc` is in a state where the main parts are there (code analysis and generation) and we are successfully generating code.

The language itself supports a narrow set of features for now, not enough for serious web app development, but we believe it is a good basis to build upon.  
The next step is to expand the language (and compiler) so it can serve the role of a smart code generator - it will still not have enough features to keep you from ejecting for a very long time, but it will be enough to give you a quick start.  
Finally, as it grows further, it should allow the development of web apps of increasing complexity without the need to eject!

While the idea is to support multiple web tech stacks in the future, right now we are focusing on the specific stack: React + react-query, NodeJS + ExpressJS, Prisma. We might yet change that as time goes, taking trends into account, but for now, this is serving us well for the purpose of developing compiler and language.


# Contributing

Any way you want to contribute is a good way :)!

Since we are so early, the best way might be to join us on discord [![Discord](https://img.shields.io/discord/686873244791210014?label=chat%20on%20discord)](https://discord.gg/rzdnErX) to discuss how you could contribute the best, and we can also discuss about language design, new features, how to improve existing code, what do you think about Wasp in general, anything really.

Even if you don't plan to submit any code, just joining discussion and giving your feedback is already great and helps a lot (motivates us and helps us figure out how to shape Wasp)!

You can also:
 - :star: Star this repo to show your interest/support.
 - :mailbox: Stay updated by subscribing to our [email list](https://wasp-lang.dev#signup).

For more technical details on building and contributing to `waspc` check [waspc/README.md](waspc/README.md).

# FAQ

Check out https://wasp-lang.dev/#faq.
