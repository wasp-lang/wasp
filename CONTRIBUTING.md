# Contributing to Wasp

Wasp's compiler is built with Haskell and under the hood it generates a web application in React and NodeJS. Given that, there are several ways in which you can contribute:

- [Wasp compiler/CLI/LSP internals](#wasp-compilerclilsp-internals) (Haskell)
- [Wasp as a web framework](#wasp-as-a-web-framework) (React, Node, HTML/CSS, database and so on)
- [Tutorials or Example apps](#tutorials-or-example-apps)
- [Documentation](#documentation)

## Before you begin

- Check out the [**Getting Started**](https://wasp.sh/docs) guide to get familiar with Wasp's fundamentals. Ideally, you'd also build an app from the [**Pick a Tutorial**](https://wasp.sh/docs/tutorials/todo-app) page to really get a feel for it!
- Figure out what you'd like to help with. It can be code, documentation, tutorials, etc. More on this is below.
- Join our Discord [![**Discord**](https://img.shields.io/discord/686873244791210014?label=chat%20on%20discord)](https://discord.gg/rzdnErX) for faster communication and feedback. We'd be happy to help you find the issue you'll enjoy working on, depending on your interests and skill set!
- Below you can find links to the good first issues. If you'd like to filter the issues on your own — please, use [this link](https://github.com/wasp-lang/wasp/issues)

Let's jump right in!

## Wasp compiler/CLI/LSP internals

Wasp compiler is implemented in Haskell, but you will also see a lot of Javascript and other web technologies because Wasp compiles it's own code into them.

While you will need to know some Haskell, you don't have to be an expert in Haskell to contribute or understand the code since we don't use complicated Haskell features much -> most of the code is relatively straightforward, and we are happy to help with the part that is not.

Check the [**Wasp compiler README**](https://github.com/wasp-lang/wasp/blob/main/waspc/README.md) for all the detailed instructions and guides.

[**Haskell-related issues for beginners can be found here.**](https://github.com/wasp-lang/wasp/issues?q=is%3Aopen+is%3Aissue+label%3A%22good+first+issue%22+label%3Ahaskell)

Feel free to contact us via Discord to ask for an appropriate issue for yourself, or just open a new one if you have something specific in mind and it isn't already there!

## Wasp as a web framework

Wasp is a language for developing full-stack web apps. This means there are plenty of tasks related to web development itself.

[**Web dev issues for beginners can be found here.**](https://github.com/wasp-lang/wasp/issues?q=is%3Aopen+is%3Aissue+label%3A%22good+first+issue%22+label%3Awebdev)

## Tutorials or Example apps

Another great way to help is to create an app with Wasp! We have an [Examples](https://wasp.sh/docs/examples) section on our website, as well as the [Tutorials](https://wasp.sh/docs/tutorials/todo-app) page. Both of them can be improved and updated with your projects.

All that's required is to create an app. And make a tutorial or a blog post to help other people reproduce your work. Some prominent examples are: [Waspello](https://wasp.sh/blog/2021/12/02/waspello), [Waspleau](https://wasp.sh/blog/2022/01/27/waspleau), [It Wasps on My Machine](https://wasp.sh/blog/2022/09/05/dev-excuses-app-tutrial) and [To-Do app](https://wasp.sh/docs/tutorials/todo-app).

Or you can re-build your existing pet project with Wasp. That would be cool!

## Documentation & Blog

It may sound like the simplest one, but it's super valuable! If you've found an issue, a broken link or if something was unclear on our [website](https://wasp.sh/) - please, feel free to fix it :)

Please make sure to **base your feature branches and PRs on the `release` branch** instead of `main`, since that's the one that is deployed to the website.

[**Documentation issues for beginners can be found here.**](https://github.com/wasp-lang/wasp/issues?q=is%3Aopen+is%3Aissue+label%3A%22good+first+issue%22+label%3Adocumentation)

If you'd like to write a blog post about Wasp, please contact us via [Discord](https://discord.gg/zKFDFrsHa9) to discuss the topic and the details.

Happy hacking!

# Policies

These are some general policies that we follow when it comes to contributions. They are not meant to be strict or exhaustive, but rather to give you a sense of what we value and expect. If you are linked here from a PR, it means that we think your contribution could be improved in some way, and following these guidelines is the best way to do it.

## AIs and LLMs

**You are free to use AI/LLM tools and agents, but every contribution must come from a human. The human must have read it, run it, understand it, and they must stand behind the contribution.** AIs can generate code faster that we can review it, and they're not always right. Everything that lands in Wasp becomes ours to maintain, so we hold a high bar for what makes it in.

We also have agents; and because we work on Wasp every day, we can generally prompt them faster and better than an outside contributor can, and review them more easily. Therefore, just asking your agent for code and then sending it to us without review is not a good use of our time, nor your tokens.

By just seeing a PR, we can't definitively know whether a human has supervised it. So **your job is to convince us that your contribution is worth it, by communicating clearly and demonstrating understanding and care**. This might not necessarily be about the immediate measurable qualities of the work; but about trust that it is going to save us time also in the long run. Tell-tale signs of an automatically generated PR will make it hard for us to trust it, and will likely lead to it being closed.

> [!NOTE]
> We will close PRs that we think don't clear these bars, simply to protect our time. If you think we misjudged yours, tell us and we'll gladly take another look. We're all human in the end, even if AI-assisted.

We're genuinely happy to mentor newcomers and help them improve. But we're not happy to see drive-by PRs that the author can't explain or won't follow up on. If you open one, be ready to respond to review, answer questions, and address feedback.

Before you open a PR, make sure you (the human) do these:

- **Understand both the code change and its motivation.** You should be able to explain what it does and why.
- **Test it locally in a real context.** Depending on the type of change, that might mean compiling Haskell code, creating real-looking tests, and/or running it in an actual Wasp app and manually clicking buttons.
- **Review it thoroughly, and make changes as needed.** The code might be correct, but is it clear? Does it use tools or abstraction that we already have in the code?
- **Communicate clearly that you've done the above, and what you found.** Fill out the [pull request template](./.github/pull_request_template.md), do not remove it, and represent your changes and process honestly.

Please don't do the following:

- **Don't set autonomous tools to do work on the Wasp repo without oversight.** A bunch of slop PRs might get your account blocked and/or reported to GitHub.
- **Don't send LLM output that you haven't reviewed and understood.** Especially, don't just copy-paste messages back and forth between an LLM and a PR. Understand what is being said or asked.
- **Don't send code you haven't run locally, in a relevant context.** Don't have your AI hand-craft files that just make it look right.
- **Don't hand-edit generated files, such as E2E snapshots.** This rule is already explained in our [CLAUDE.md](./CLAUDE.md), but lots of slop PRs do not seem to have read it.
