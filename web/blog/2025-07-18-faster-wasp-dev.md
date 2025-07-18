---
title: "Cleaning up 5 years of tech debt in a full-stack JS framework"
authors: [cprecioso]
image: /img/faster-wasp-dev/da-boi-in-a-fast-car.png
tags: [wasp, launch-week, inside-wasp, open-source]
---

![Wasp's mascot, "Da Boi", driving a fast, pimped ride through a blurred city](/img/faster-wasp-dev/da-boi-in-a-fast-car.png)

> We’re building Wasp, a full-stack web framework the sweats the details and the boilerplate, so you go straight into building. It’s like having a senior engineer set up your architecture (server APIs, client routing, authentication, async jobs, and more!), so you focus only in the interesting parts of **your** app. [Try it out!](https://wasp.sh/docs/quick-start)

Have you ever heard the expression [_the cobbler's children alway go barefoot_](https://en.wiktionary.org/wiki/the_shoemaker%27s_children_go_barefoot)? Lately, it has felt a little too close to home. We’re so focused on working for you, that at times we forgot to work for ourselves!

<!-- truncate -->

But [in this Launch Week](./2025-07-07-wasp-launch-week-10.md), the team has taken some time between shipping features and fixing bugs, and looked at the tools we were using ourselves. What we found _won't_ surprise you: the process by which we build Wasp needs a serious tune-up. Our features have changed a lot from the initial to-do apps, to the real projects that are shipping today; made by hobbyists, startups, and enterprise. Now, we need to update our approach for our current scale, without losing our open identity.

**So… we’re giving you a tour!**

As an [open-source project](https://github.com/wasp-lang/wasp) you can see all of our warts (and all of our genius code) by yourself, whenever you want. But today we’re taking you on a guided tour as we inspect our own engine. It’s about time your cobblers stopped walking around barefoot and get themselves some decent shoes!

## First stop: coding faster

The speed at which we can write robust, testable code directly impacts the speed at which we can deliver new features to you. Our foundation is functional, but as we're ramping up, some parts are holding us back. We've targeted two key areas for a complete overhaul.

### From brittle templates…

**Here’s a fun fact:** the Wasp compiler creates your app by analyzing your code and processing a bunch of Mustache templates[^1]:

[^1]: Hey, that's where our `=}` logo comes from!

```mustache
const defaultViteConfig = {
  base: "{= baseDir =}",
  plugins: [
    validateEnv(),
    react(),
    detectServerImports(),
  ],
  server: {
    port: {= defaultClientPort =},
    host: "0.0.0.0",
    open: true,
  },
  // etc
}

```

When you run `wasp start`, we read your `.wasp` files and stitch together your app by surgically inserting your code into these templates. This approach got us off the ground, but it can be painful[^2]. Here, let me show you what that template actually looks like in our editors:

[^2]:
    While preparing this blog post, [Matija](https://x.com/MatijaSosic) dropped by and imparted some Wasp lore on me:

    > We didn’t start with libraries immediately because we thought it’s important to generate human-readable code. It was also one of the reasons that got people to try Wasp in Alpha, because they knew they won’t get locked in.
    >
    > In reality, it didn’t matter even then - almost nobody “ejected” the code, even in Alpha. And now it’s completely irrelevant.

![A screenshot of a code editor with the same code as above, showing lots of syntatic and static analysis errors](/img/faster-wasp-dev/bad-editor.png)

Yeesh... templates are very unpleasant to maintain at scale. Can you think of all the nice features you have in modern JS land?

- Linting
- Import checking
- Automated refactoring
- Autocompletion
- Type-checking 🫨

Yeah, we have a hard time with those. With templates you can't easily test in isolation, reasoning about logic is a chore, and editor tooling offers little help.

To ensure a change doesn't break anything, we usually have to _build an entire Wasp app_ that uses a specific feature just to test and type-check the generated code downstream. At times, it is slow and painful, a cycle of "change, generate, dig through the output". And unsurprisingly, this dance discourages community contributors.

### …to bulletproof libraries

So now, we’re planning to systematically pull this system apart, and **replace our template pile with standalone TypeScript libraries**. Just normal libraries. This way, we can work on them with familiar, battle-tested tools. Instead of templating thousands of lines of code, the Wasp compiler will only have to stitch together our provided libraries, for different features of your app. Maybe in just a couple of lines.

:::tip TanStack Router - thank you for being a role model!

A key inspiration for this approach is [TanStack Router](https://tanstack.com/router/) and [its `routeTree.gen.ts` approach](https://github.com/TanStack/router/blob/main/examples/react/quickstart-file-based/src/routeTree.gen.ts). They use code generation not to create the application logic itself, but only to wire together user code with their own library code. They've gone [extremely deep into making the experience fast and pleasant](https://tanstack.com/blog/tanstack-router-typescript-performance), and we're learning a lot from them.

:::

For you, our users, this process will be entirely transparent. We’re still taking advantage of having a smart compiler that knows about your app. And we're not planning to make you install a hundred different libraries, don't worry! The Wasp libraries will be a completely internal implementation detail. But thanks to it, you'll see your builds becoming faster and more robust.

This is a big undertaking, but the payoff will be a more stable, testable, and performant Wasp compiler. For us, it means faster, more confident development. For you, it means a more reliable tool that we can build upon for years to come. You can follow this epic refactor on our [public GitHub issue](https://github.com/wasp-lang/wasp/issues/2668).

### Killing the app sprawl

Just a few months ago, our internal project landscape had become a sprawling affair. We had three distinct starter templates scattered across [two](https://github.com/wasp-lang/starters/tree/wasp-v0.16-template) [different](https://github.com/wasp-lang/wasp/tree/v0.16.0/waspc/data/Cli/templates) repositories; plus [four](https://github.com/wasp-lang/wasp/tree/v0.16.0/waspc/examples) separate, independent "example" apps that we used for internal testing.

That was... annoying. Keeping everything in sync with the latest version of Wasp was a manual chore. A change in the compiler required hopping between repos, and hoping we didn't miss anything-a perfect recipe for version drift.

But, when Earth most needed them, [Franjo](./2025-03-20-meet-franjo-engineer-at-wasp.md) and [Miho](./2024-12-24-meet-miho-founding-engineer-wasp.md) came through!

First, we consolidated the starters. We now have two, and only two, official starters, living directly inside the main Wasp repo: one for a basic app with guidance on enabling common features, and a minimal one for users who know exactly what they want. Gone are the super-specialized starters that saw basically no usage. Don't worry though, [OpenSaas](https://opensaas.sh/) and [Mage](https://usemage.ai/) are still offered in `wasp new` to help you with more advanced needs.

![A screenshot of a terminal running `wasp new`, asking the user to choose between 4 different starters: `basic`, `minimal`, `saas`, and `ai-generated`.](/img/faster-wasp-dev/new-wasp-new.png)

<small>
When you run `wasp new`, this is what you see, starting from Wasp 0.17
</small>

Second, our four separate test apps have been merged into a single, comprehensive ["kitchen sink" example project](https://github.com/wasp-lang/wasp/tree/main/waspc/examples/todoApp). And instead of having to know a route for a specific test, the app is well laid out and explained. And this app is covered by a suite of end-to-end tests that run against every single pull request.

![A screenshot of our Kitchen Sink app, focusing on its navigation sidebar, with multiple tests easily accessible.](/img/faster-wasp-dev/todo-app-sidebar.png)

<small>Who knew a sidebar would alleviate navigation issues 💁‍♀️</small>

### Monorepo for the win!

By co-locating the compiler, starters, and test apps in a single repo, everything evolves in lockstep. We can make a change to the compiler and instantly update and validate it against our starters and test suite. No more context switching, no more repo-hopping, no more "did we update the X starter?". Just a clean, consolidated, and thoroughly tested codebase.

## Second stop: testing faster

Testing shouldn't be an afterthought, but it also shouldn't be a pain. We realized our internal testing workflows were full of friction, friction that we could remove not just for ourselves, but for every Wasp developer.

### Introducing `wasp build start`, a dress rehearsal for your app

<div style={{float: "right", maxWidth: "50%", margin: "1em"}}>

![Wasp's mascot, "Da Boi", with a wig, on a stage, with a spotlight](/img/faster-wasp-dev/da-boi-dress-rehearsal.png)

</div>

How do you _really_ know your app is ready for production? The `wasp start` dev server is great for development, but a production build is a different beast - assets are bundled and optimized, environment variables are handled differently, and the server runs in a hardened mode.

Internally, our method for testing this is a collection of [ad-hoc scripts](https://github.com/wasp-lang/wasp/issues/1883#issuecomment-2766265289). First of all, these rely on you knowing [Filip](./2022-05-31-filip-intro.md) and asking him for his scripts. And second, they depend on our developer machines’ setups, and that's not something we could ever ship to you. We need a first-class solution.

That’s why we’re developing a new CLI command: `wasp build start`. This command will take your built production output (the result of `wasp build`) and start your app locally, using the generated client files, and the server Docker container. It’s a one-step dress rehearsal for your deployment.

This lets you debug production-only issues on your own machine, long before your users do, running the app in the same way that it runs when deployed. That is perfect for the internal team, but the tool will also be available to everyone soon. You can track its development [here](https://github.com/wasp-lang/wasp/issues/1883).

<div style={{clear: "both"}}></div>

### Smooth deployment makes a smooth launch

<div style={{float: "right", maxWidth: "50%", margin: "1em"}}>

![image.png](/img/faster-wasp-dev/github-actions-deploy.png)

</div>

Wasp aims to make deployment a breeze, but how could we be sure our deployment adaptors for providers like [Fly.io](http://fly.io/) and [Railway](https://railway.com/) were always up-to-date? A [subtle API change on their end](https://github.com/wasp-lang/wasp/pull/2760) or a small bug in our Dockerfile could break the process without us knowing. Of course, we test it, but doing it manually can risk us missing stuff.

To solve this, we built an automated testing pipeline specifically for deployments. This system will automatically deploy our example Wasp apps to our supported providers on a regular basis. It will act as a continuous smoke test for our entire deployment story.

This means we can:

- Instantly catch regressions caused by our own changes.
- Proactively detect breaking changes from hosting providers.
- Guarantee that our documentation and deployment templates are always accurate and working.

We have some example apps that we deploy with the latest changes:

- [Waspello](https://waspello-app-client.fly.dev/) (from [this blog post](./2021-12-02-waspello.md))
- [Waspleau](https://waspleau-app-client.fly.dev/) (from [this blog post](./2022-01-27-waspleau.md))
- [Websockets Voting](https://websockets-voting-client.fly.dev/) (from [this blog post](./2023-08-09-build-real-time-voting-app-websockets-react-typescript.md))

This isn't just about testing our code; it's about testing the entire ecosystem you rely on. You can see the plan for this initiative on [GitHub](https://github.com/wasp-lang/wasp/issues/2831).

<div style={{clear: "both"}}></div>

## Third stop: release faster

![Wasp's mascot, "Da Boi", reading our super long release checklist with steps and sub-steps, with a worried face](/img/faster-wasp-dev/scared-to-release.png)

Our current release process is, to put it mildly, **long**. You can see the [checklist for yourself](https://github.com/wasp-lang/wasp/blob/main/waspc/README.md#typical-release-process). It's a long document with dozens of manual steps:

- Running local scripts
- Bumping version numbers across npm and cabal files
- Drafting changelogs
- Creating git tags
- Testing apps
- ...and more

A major release can easily consume a full day of an engineer's time. If bugs are found, it can extend to more days and more engineers. It is a tedious, high-stakes process where one wrong move derails the whole thing. This manual toil can be a bottleneck - and from the ones I've done, nerve-wracking!

We are now on a mission to automate every possible step. We're building a new system to handle the entire process. This involves automatically bumping versions, creating the changelog, testing everything, and publishing our releases.

The goal is to transform our release process from a day-long manual slog into a single, push-button operation. This means more frequent, more reliable releases, getting bug fixes and new features out of our door and into your hands faster than ever. Follow the automation effort [here](https://github.com/wasp-lang/wasp/issues/2662).

## Now it's your turn to go fast

We're sharpening our own tools so we can build better ones for you. These internal improvements -a modern codebase, streamlined testing, and automated releases- are all designed to shorten the distance between an idea and a shipped feature in your hands.

And because Wasp is an open-source citizen, we can take advantage of our community input, learn from other frameworks, and work in the open.

We've shown you our warts, but the cobbler's workshop is finally making some top-quality shoes for the family. We're getting faster so you can be faster.

Now, go build something amazing. [**Try Wasp today**](https://wasp.sh/) and see how fast you can really move.

---
