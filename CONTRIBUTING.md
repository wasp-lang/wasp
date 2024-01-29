# Contributing to Wasp

Wasp's compiler is built with Haskell and under the hood it generates a web application in React and NodeJS. Given that, there are several ways in which you can contribute: 
- [Wasp compiler/CLI/LSP internals](#wasp-compilerclilsp-internals) (Haskell)
- [Wasp as a web framework](#wasp-as-a-web-framework) (React, Node, HTML/CSS, database and so on)
- [Tutorials or Example apps](#tutorials-or-example-apps)
- [Documentation](#documentation)

## Before you begin 

- Check out the [**Getting Started**](https://wasp-lang.dev/docs) guide to get familiar with Wasp's fundamentals. Ideally, you'd also build an app from the [**Pick a Tutorial**](https://wasp-lang.dev/docs/tutorials/todo-app) page to really get a feel for it!
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

Another great way to help is to create an app with Wasp! We have an [Examples](https://wasp-lang.dev/docs/examples) section on our website, as well as the [Tutorials](https://wasp-lang.dev/docs/tutorials/todo-app) page. Both of them can be improved and updated with your projects. 

All that's required is to create an app. And make a tutorial or a blog post to help other people reproduce your work. Some prominent examples are: [Waspello](https://wasp-lang.dev/blog/2021/12/02/waspello), [Waspleau](https://wasp-lang.dev/blog/2022/01/27/waspleau), [It Wasps on My Machine](https://wasp-lang.dev/blog/2022/09/05/dev-excuses-app-tutrial) and [To-Do app](https://wasp-lang.dev/docs/tutorials/todo-app).

Or you can re-build your existing pet project with Wasp. That would be cool!

## Documentation & Blog

It may sound like the simplest one, but it's super valuable! If you've found an issue, a broken link or if something was unclear on our [website](https://wasp-lang.dev/) - please, feel free to fix it :)

Please make sure to **base your feature branches and PRs on the `release` branch** instead of `main`, since that's the one that is deployed to the website.

[**Documentation issues for beginners can be found here.**](https://github.com/wasp-lang/wasp/issues?q=is%3Aopen+is%3Aissue+label%3A%22good+first+issue%22+label%3Adocumentation)

If you'd like to write a blog post about Wasp, please contact us via [Discord](https://discord.gg/zKFDFrsHa9) to discuss the topic and the details.

Happy hacking!