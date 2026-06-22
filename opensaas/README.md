## Welcome to your new SaaS App! 🎉

<div style="display: flex; gap: 16px; align-items: center;">
  <a href="https://www.producthunt.com/products/open-saas?embed=true&utm_source=badge-top-post-topic-badge&utm_medium=badge&utm_source=badge-open&#0045;saas&#0045;2&#0045;0" target="_blank">
    <img src="https://api.producthunt.com/widgets/embed-image/v1/top-post-topic-badge.svg?post_id=1023519&theme=neutral&period=weekly&topic_id=237&t=1760520428563" alt="Open&#0032;SaaS&#0032;2&#0046;0 - Free&#0044;&#0032;open&#0045;source&#0032;SaaS&#0032;starter&#0032;kit&#0032;with&#0032;superpowers | Product Hunt" style="width: 250px; height: 54px;" width="250" height="54" />
  </a>
  <a href="https://www.producthunt.com/products/open-saas?embed=true&utm_source=badge-top-post-badge&utm_medium=badge&utm_source=badge-open&#0045;saas&#0045;2&#0045;0" target="_blank">
    <img src="https://api.producthunt.com/widgets/embed-image/v1/top-post-badge.svg?post_id=1023519&theme=neutral&period=daily&t=1760520428563" alt="Open&#0032;SaaS&#0032;2&#0046;0 - Free&#0044;&#0032;open&#0045;source&#0032;SaaS&#0032;starter&#0032;kit&#0032;with&#0032;superpowers | Product Hunt" style="width: 250px; height: 54px;" width="250" height="54" />
  </a>
</div>

https://github.com/user-attachments/assets/3856276b-23e9-455e-a564-b5f26f4f0e98

You've decided to build a SaaS app with the Open SaaS template. Great choice!

This template is:

1. fully open-source
2. completely free to use and distribute
3. comes with a ton of features out of the box!
4. ready to work with your favorite AI coding tool or agent (Claude Code, Cursor, Codex, OpenCode, etc.)

🧑‍💻 Check it out in action here: [OpenSaaS.sh](https://opensaas.sh)
📚 Check out the Docs here: [Open SaaS Docs](https://docs.opensaas.sh)

## What's inside?

The template itself is built on top of some very powerful tools and frameworks, including:

- 🐝 [Wasp](https://wasp.sh) - a full-stack React, NodeJS, Prisma framework with superpowers
- 🚀 [Astro](https://starlight.astro.build/) - Astro's lightweight "Starlight" template for documentation and blog
- 💸 [Stripe](https://stripe.com), [Polar.sh](https://polar.sh), or [Lemon Squeezy](https://lemonsqueezy.com/) - for products and payments
- 💅 [ShadCN UI](https://tailwindcss.com) - for components & styling (plus admin dashboard!)
- 🤖 [AI-Ready](https://docs.opensaas.sh/) - Custom Plugins, Skills, & Rules for AI-assisted coding with Claude Code, Cursor, or your favorite AI-assisted coding tool
- 📈 [Plausible](https://plausible.io) or [Google](https://analytics.google.com/) Analytics
- 🤖 [OpenAI](https://openai.com) - OpenAI API w/ function calling example
- 📦 [AWS S3](https://aws.amazon.com/s3/) - for file uploads
- 📧 [SendGrid](https://sendgrid.com), [MailGun](https://mailgun.com), or SMTP - for email sending
- 🧪 [Playwright](https://playwright.dev) - end-to-end tests with Playwright

Because we're using Wasp as the full-stack framework, we can leverage a lot of its features to build our SaaS in record time, including:

- 🔐 [Full-stack Authentication](https://wasp.sh/docs/auth/overview) - Email verified + social Auth in a few lines of code.
- ⛑ [End-to-end Type Safety](https://wasp.sh/docs/data-model/operations/overview) - Type your backend functions and get inferred types on the front-end automatically, without the need to install or configure any third-party libraries. Oh, and type-safe Links, too!
- 🤖 [Jobs](https://wasp.sh/docs/advanced/jobs) - Run cron jobs in the background or set up queues simply by defining a function in the config file.
- 🚀 [One-command Deploy](https://wasp.sh/docs/advanced/deployment/overview) - Easily deploy your DB, Server, & Client with one commaned to [Railway](https://railway.app) or [Fly.io](https://fly.io) via the CLI. Or deploy manually to any other hosting serivce of your choice.

You also get access to Wasp's diverse, helpful community if you get stuck or need help.

- 🤝 [Wasp Discord](https://discord.gg/aCamt5wCpS)

## Getting Started

### Simple Instructions

First, to install the latest version of [Wasp](https://wasp.sh/) on macOS, Linux, or Windows with WSL, run the following command:

```bash
npm i -g @wasp.sh/wasp-cli
```

Then, create a new SaaS app with the following command:

```bash
wasp new -t saas
```

This will create a **clean copy of the Open SaaS template** into a new directory, and you can start building your SaaS app right away!

### Detailed Instructions

For everything you need to know about getting started and using this template, check out the [Open SaaS Docs](https://docs.opensaas.sh).

We've documented everything in great detail, including installation instructions, pulling updates to the template, guides for integrating services, SEO, deployment, and more. 🚀

## Getting Help & Providing Feedback

There are two ways to get help or provide feedback (and we try to always respond quickly!):

1. [Open an issue](https://github.com/wasp-lang/open-saas/issues)
2. [Wasp Discord](https://discord.gg/aCamt5wCpS) -- please direct questions to the #🙋questions forum channel

## Development Tools

### Code Quality Tools

This repository includes comprehensive code quality tooling to help maintain code standards:

#### Prettier (Code Formatting)

Prettier is configured for automatic code formatting across all JavaScript, TypeScript, and other supported files.

```bash
# Check if files are formatted correctly
npm run prettier:check

# Automatically format all files
npm run prettier:format
```

#### ESLint (Code Linting)

ESLint is configured with TypeScript and React support to catch potential bugs and enforce code quality standards.

```bash
# Run ESLint to check for issues
npm run lint

# Automatically fix fixable issues
npm run lint:fix
```

The ESLint configuration includes:

- TypeScript support with `@typescript-eslint`
- React and React Hooks linting
- Sensible defaults tuned for a SaaS application
- Automatic support for CommonJS (.cjs), ES Modules (.mjs), and TypeScript files

Both Prettier and ESLint checks are automatically run in CI/CD pipelines to ensure code quality.

For information about other development tools used to maintain derived projects (like opensaas.sh and template-test), see [tools/README.md](./tools/README.md).

## Contributing

Note that we've tried to get as many of the core features of a SaaS app into this template as possible, but there still might be some missing features or functionality.

We could always use some help tying up loose ends: contributions are welcome! Check out [CONTRIBUTING.md](/CONTRIBUTING.md) for more details.
