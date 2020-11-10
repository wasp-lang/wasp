<p align=center>
  <img height="80px" src="https://user-images.githubusercontent.com/1536647/77317442-78625700-6d0b-11ea-9822-0fb21e557e87.png"/>
</p>
<p align=center>
  <span>A programming language that understands what a web app is.</span>
</p>
<br>
<p align=center>
  <a href="https://github.com/wasp-lang/wasp/blob/master/LICENSE"><img alt="license" src="https://img.shields.io/github/license/wasp-lang/wasp"></a>
  <a href="https://github.com/wasp-lang/wasp/search?l=haskell"><img alt="language" src="https://img.shields.io/badge/language-Haskell-purple.svg"></a>
  <a href="https://github.com/wasp-lang/wasp/actions"><img alt="build status" src="https://img.shields.io/github/workflow/status/wasp-lang/wasp/CI"/></a>
  <a href="https://discord.gg/rzdnErX"><img alt="discord" src="https://img.shields.io/discord/686873244791210014?label=chat%20@%20discord"/></a>
</p>

------

- [**Project page**](https://wasp-lang.dev)
- [**Docs**](https://wasp-lang.dev/docs)

<br>

Wasp (**W**eb **A**pplication **Sp**ecification Language) is an extensible [DSL](https://en.wikipedia.org/wiki/Domain-specific_language) (domain-specific language) for building modern web apps with less code.

Concepts such as *app*, *page*, *user*, *login*, *frontend*, *production*, etc. are baked into the language, bringing a new level of expressiveness and allowing you to get more work done with fewer lines of code.

NOTE: Wasp is still in alpha, meaning it has bugs, and many critical features are still missing and it is still changing a lot!

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

entity Task {=psl
    id          Int     @id @default(autoincrement())
    description String
    isDone      Boolean @default(false)
psl=}
```

Source files (`.wasp`, `.js(x)`, `.css`, ...) are compiled (transpiled) by `waspc` (Wasp compiler) into the web technology stack of your choice (e.g. React + Redux + ExpressJS + ...).

:arrow_forward: Check out [TodoApp example](waspc/examples/todoApp) for complete code example. :arrow_backward:

Why is Wasp awesome:
- **Quick start**: Due to its expressiveness, you can create and deploy a production-ready web app from scratch with very few lines of concise, consistent, declarative code.
- **Flexible**: When you need more control than Wasp offers, you can write code in existing technologies such as js/html/css/... and combine it with Wasp code!
- **No lock-in**: If Wasp becomes too limiting for you, simply eject and continue with the generated source code, which is human-readable.

For more information about Wasp, check [**docs**](https://wasp-lang.dev/docs).

# Get started
Check out [the docs](https://wasp-lang.dev/docs/tutorials/getting-started).

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

The best place to start is to check out [waspc/](waspc/), where you can find detailed steps for the first time contributors + technical details about the Wasp compiler.

Even if you don't plan to submit any code, just joining the discussion on discord [![Discord](https://img.shields.io/discord/686873244791210014?label=chat%20on%20discord)](https://discord.gg/rzdnErX) and giving your feedback is already great and helps a lot (motivates us and helps us figure out how to shape Wasp)!

You can also:
 - :star: Star this repo to show your interest/support.
 - :mailbox: Stay updated by subscribing to our [email list](https://wasp-lang.dev#signup).

# FAQ

Check out https://wasp-lang.dev/#faq.

# License

MIT License

Copyright (c) 2020 wasp-lang

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
