```
                                                                 _  _
                             __      ____ _ ___ _ __            | )/ )
                             \ \ /\ / / _` / __| '_ \        \\ |//,' __
                              \ V  V / (_| \__ \ |_) |       (")(_)-"()))=-
                               \_/\_/ \__,_|___/ .__/           (\\
                                               |_|
```



<p align=center> A programming language that understands what a web app is. </p>
<br>

[![Join the community on Spectrum](https://withspectrum.github.io/badge/badge.svg)](https://spectrum.chat/wasp)

- **Project page**: [https://wasp-lang.dev]
- **Demo**: [https://wasp-lang.dev/#demo]

<br>

Wasp (**W**eb **A**pplication **Sp**ecification Language) is an extensible [DSL](https://en.wikipedia.org/wiki/Domain-specific_language) (domain-specific language) for building modern web apps with less code.

Concepts such as *app*, *page*, *user*, *login*, *frontend*, *production*, etc. are baked into the language, bringing a new level of expressiveness and allowing you to get more work done with fewer lines of code.

```js
// todoApp.wasp:

app TodoApp {
  title: "ToDo app",
  favicon: "./todo-logo.png"
}

entity Task {
  description :: string,
  isDone :: boolean
}

entity-list<Task> TaskList {
  editable: true
}

page Main {
  route: "/",
  content: {=jsx
    <h1>Todos</h1>
    <TaskList/>
  jsx=},
  style: "./main.css"
}

...
```

Source files (`.wasp`, `.js(x)`, `.css`, ...) are compiled (transpiled) by `waspc` (Wasp compiler) into the web technology stack of your choice (e.g. React + Redux + ExpressJS + ...).

:arrow_forward: Check out [TodoApp example](examples/todoApp) for complete code example. :arrow_backward:

Why is Wasp awesome:
- **Quick start**: Due to its expressiveness, you can create and deploy a production-ready web app from scratch with very few lines of concise, consistent, declarative code.
- **Flexible**: When you need more control than Wasp offers, you can write code in existing technologies such as js/html/css/... and combine it with Wasp code!
- **No lock-in**: If Wasp becomes too limiting for you, simply eject and continue with the generated source code (in web tech stack of your choice) that follows industry best-practices, as if it was handwritten by a senior engineer.

For more of general information about Wasp, check:
- **The main webpage**: [https://wasp-lang.dev].
- [**Blog**](https://blog.wasp-lang.dev/): [Why are we building Wasp?](https://blog.wasp-lang.dev/posts/2019-09-01-hello-wasp.html)

# Get started

In `waspc` directory is Wasp compiler which you can run and build if you want, however it is still under heavy development and will not give great experience of trying out Wasp: check [Project status](#project-status) for more info.

If you want to be notified when we release the version which will be nice and easy to try, either watch the repo or subscribe to our [email list](https://wasp-lang.dev#signup).

If you want to get involved more or just chat with us, check [Contributing](#contributing)!


# This repository

This is the main repo of the Wasp universe, containing core code (mostly `waspc` - Wasp compiler) and the supporting materials.


# Documentation

We don't have proper documentation yet due to how fast everything is changing in this early stage of Wasp development.

In the meantime, the best way to learn more is by taking a look at our [TodoApp example](examples/todoApp), or you can ask us questions directly!


# Project status

We are still very early (pre-alpha). Specifically, we are developing `waspc` (Wasp compiler) and also designing language as we go.

Currently, `waspc` is in a state where the main parts are there (code analysis and generation) and we are successfully generating code as can be seen at [TodoApp example](examples/todoApp).

The language itself supports a narrow set of features for now, not enough for serious web app development, but we believe it is a good basis to build upon.  
The next step is to expand the language (and compiler) so it can serve the role of a powerful code generator - it will still not have enough features to keep you from ejecting for a very long time, but it will be enough to give you a quick start.  
Finally, as it grows further, it should allow the development of web apps of increasing complexity without the need to eject!

While the idea is to support multiple web tech stacks in the future, right now we are focusing on the specific stack: React + Redux, NodeJS + ExpressJS, Mongo. We might yet change that as time goes, taking trends into account, but for now, this is serving us well for the purpose of developing compiler and language.


# Contributing

Any way you want to contribute is a good way :)!

Since we are so early, the best way might be to join us on spectrum [![Join the community on Spectrum](https://withspectrum.github.io/badge/badge.svg)](https://spectrum.chat/wasp) to discuss how you could contribute the best, and we can also discuss about language design, new features, how to improve existing code, what do you think about Wasp in general, anything really.

Even if you don't plan to submit any code, just joining discussion and giving your feedback is already great and helps a lot (motivates us and helps us figure out how to shape Wasp)!

You can also:
 - :star: Star this repo to show your interest/support.
 - :mailbox: Stay updated by subscribing to our [email list](https://wasp-lang.dev#signup).

# FAQ

Check out https://wasp-lang.dev/#faq.


# Other

Wasp ASCII art used in the title is from https://www.asciiart.eu/animals/insects/bees, author: Stef00.
