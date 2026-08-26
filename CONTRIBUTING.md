# Contributing to Wasp

There are several main ways in which you can contribute to Wasp:

- [Wasp as a web framework](#wasp-as-a-web-framework) (React, Node, HTML/CSS, database and so on)
- [Wasp CLI/compiler internals](#wasp-clicompiler-internals) (Haskell)
- [Tutorials or Example apps](#tutorials-or-example-apps)
- [Documentation](#documentation)

## Before you begin

- Get some experience with using Wasp, if you don't have any yet.
  Check out the [**Getting Started**](https://wasp.sh/docs) guide to get familiar with Wasp's fundamentals.
  Ideally, you'd also build an app from the [**Pick a Tutorial**](https://wasp.sh/docs/tutorials/todo-app) page to really get a feel for it!
- Figure out what you'd like to help with. It can be code, documentation, tutorials, etc. Check [Ways to contribute](#ways-to-contribute) for more details.
- Join our Discord [![**Discord**](https://img.shields.io/discord/686873244791210014?label=chat%20on%20discord)](https://discord.gg/rzdnErX) for faster communication and feedback. We'd be happy to help you find the issue you'll enjoy working on, depending on your interests and skill set! `#wasp-dev` channel is the perfect place to ping us with the task you want to do and how you plan to do it, which reduces duplicate or misdirected efforts.

## This repo

We are using a monorepo approach, where one git repo contains multiple related projects.

In our case, those are [web](web/) (Wasp's web page, including docs and blog), [waspc](waspc/) (Wasp CLI & framework), [examples](examples/) (example Wasp apps), etc.

While this document captures the general instructions for the whole repo, make sure to check the README of each individual project in the repo when working on it for more detailed instructions.

## Dev tooling

### Setup

<!-- prettier-ignore -->
> [!NOTE]
> **Developing on Windows?** Use the Bash shell bundled with [Git for Windows](https://git-scm.com/download/win) (often called "Git Bash"). Wasp's development scripts are Bash scripts and won't run in PowerShell or Command Prompt. If you develop inside WSL (Windows Subsystem for Linux), you are effectively on Linux, so follow the Linux instructions instead.

We use [mise](https://mise.jdx.dev/) to manage our development tools (e.g. Haskell, Node, and code formatters). Mise is an all-in-one tool that makes it easy to set up and manage all the different tools needed for the Wasp repo. Everything is declared in a single file ([`mise.toml`](mise.toml)), and every developer can use it to set up their environment in a consistent way. We also use it on our CI to ensure it uses the same versions of tools as well.

Run `mise install` from the root of the repo to install all the required tools. Then, you can access the mise-managed tools in different ways:

- **(Recommended for local development)** You can set up your shell to automatically call the `mise activate` script. This will make sure that the specified tools and versions are in your `PATH` when you go into the repo. Check their installation instructions at https://mise.jdx.dev/installing-mise.html#shells.

- You can also run [`mise en`](https://mise.jdx.dev/cli/en.html) to go into a one-off shell for the current project, similar to `nix-shell` or `virtualenv`.

- If you don't want to add a shell hook, you can use the [Shims mode](https://mise.jdx.dev/dev-tools/shims.html), which lets you just add a single directory to your `PATH`, which will get populated with intelligent redirectors to the correct versions of the tools for the current working directory.

- For one-off commands, you can use [the `mise exec` command](https://mise.jdx.dev/cli/exec.html) (or `mise x`) to run a specific command with the repo tools available, e.g. `mise x -- ghc --version`, `mise x -- node --version`, `mise x -- ./run build`, etc.

You can learn more and install Mise by following the [official instructions](https://mise.jdx.dev/getting-started.html), then run `mise install` from the repo root to install the required tools.

> [!NOTE]
> There are no hard dependencies on mise for local development, so if you prefer to use your own tooling, you can install each program separately, and use the versions specified in [`mise.toml`](mise.toml) as a reference. But then, you're in charge of making sure you have the right versions of the tools installed, and keeping them up-to-date as we upgrade them.

### Basic commands

Formatting is defined at the repo level, and you can run it from the repo root with

```
npm run format:check
```

to check the formatting without any changes to the files, or

```
npm run format:write
```

to check and automatically fix formatting (modifies the files).

## Ways to contribute

### Wasp as a web framework

Wasp is a language for developing full-stack web apps. This means there are plenty of tasks related to web development itself, and most come down to improving one of the core features of Wasp (like Auth, Operations, Jobs, Rendering), or maybe improving our dev tooling config, etc.

If you have full-stack experience outside of Wasp, you should already be in a good place to start contributing in this direction.

[**Web dev issues for beginners can be found here.**](https://github.com/wasp-lang/wasp/issues?q=is%3Aopen+is%3Aissue+label%3A%22good+first+issue%22+label%3Awebdev)

### Wasp CLI/compiler internals

The Wasp compiler is implemented in Haskell, but you will also see a lot of Javascript and other web technologies because Wasp compiles its own code into them.

While you will need to know some Haskell, you don't have to be an expert in Haskell to contribute or understand the code since we don't use complicated Haskell features often. Most of the code is relatively straightforward, and we are happy to help with the part that is not.

Check the [**Wasp compiler README**](https://github.com/wasp-lang/wasp/blob/main/waspc/README.md) for all the detailed instructions and guides.

### Tutorials or Example apps

Another great way to help is to create an app with Wasp! We have an [Examples](https://wasp.sh/docs/examples) section on our website, as well as the [Tutorials](https://wasp.sh/docs/tutorials/todo-app) page. Both of them can be improved and updated with your projects.

All that's required is to create an app. And make a tutorial or a blog post to help other people reproduce your work. Some prominent examples are: [Waspello](https://wasp.sh/blog/2021/12/02/waspello), [Waspleau](https://wasp.sh/blog/2022/01/27/waspleau), and [To-Do app](https://wasp.sh/docs/tutorials/todo-app).

Or you can re-build your existing pet project with Wasp. That would be cool!

### Documentation & Blog

It may sound like the simplest one, but it's super valuable! If you've found an issue, a broken link or if something was unclear on our [website](https://wasp.sh/) - please, feel free to fix it :)

Please make sure to **base your feature branches and PRs on the `release` branch** instead of `main`, since that's the one that is deployed to the website.

[**Documentation issues for beginners can be found here.**](https://github.com/wasp-lang/wasp/issues?q=is%3Aopen+is%3Aissue+label%3A%22good+first+issue%22+label%3Adocumentation)

If you'd like to write a blog post about Wasp, please contact us via [Discord](https://discord.gg/zKFDFrsHa9) to discuss the topic and the details.

Happy hacking!

## Policies

These are some general policies that we follow when it comes to contributions. They are not meant to be strict or exhaustive, but rather to give you a sense of what we value and expect. If you are linked here from a PR, it means that we think your contribution could be improved in some way, and following these guidelines is the best way to do it.

### AIs and LLMs

**You are free to use AI/LLM tools and agents. But the standard remains the same: you must own and stand behind every line of code and every decision in your PR, exactly as if no AI was involved.** That means you've read it, run it, understood it, and you can explain and defend it under review.

Everything that lands in Wasp becomes ours to maintain, so the bar is the same regardless of how the code was written. If a PR shows clear signs that no human supervised it, we'll close it to protect our time. If you think we got that wrong, just tell us and we'll take another look.
