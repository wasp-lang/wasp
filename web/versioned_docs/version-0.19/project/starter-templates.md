---
title: Starter Templates
---

We created a few starter templates to help you get started with Wasp. Check out the list [below](#available-templates).

## Using a Template

Run `wasp new` to run the interactive mode for creating a new Wasp project.

It will ask you for the project name, and then for the template to use:

```
$ wasp new
Enter the project name (e.g. my-project) ▸ MyFirstProject
Choose a starter template
[1] basic (default)
    Simple starter template with a single page.
[2] todo-ts
    Simple but well-rounded Wasp app implemented with Typescript & full-stack type safety.
[3] saas
    Everything a SaaS needs! Comes with Auth, ChatGPT API, Tailwind, Stripe payments and more. Check out https://opensaas.sh/ for more details.
[4] embeddings
    Comes with code for generating vector embeddings and performing vector similarity search.
[5] ai-generated
    🤖 Describe an app in a couple of sentences and have Wasp AI generate initial code for you. (experimental)
 ▸ 1

🐝 --- Creating your project from the "basic" template... -------------------------

Created new Wasp app in ./MyFirstProject directory!

To run your new app, do:
    cd MyFirstProject
    wasp db start
```

## Available Templates

When you have a good idea for a new product, you don't want to waste your time on setting up common things like authentication, database, etc. That's why we created a few starter templates to help you get started with Wasp.

### OpenSaaS.sh template

![SaaS Template](/img/starter-templates/open-saas-banner.png)

Everything a SaaS needs! Comes with Auth, ChatGPT API, Tailwind, Stripe payments and more. Check out https://opensaas.sh/ for more details.

**Features:** Stripe Payments, OpenAI GPT API, Google Auth, SendGrid, Tailwind, & Cron Jobs

Use this template:

```
wasp new <project-name> -t saas
```

### Vector Similarity Search Template

![Vector Similarity Search Template](/img/starter-templates/embeddings-client.png)

A template for generating embeddings and performing vector similarity search on your text data!

**Features:** Embeddings & vector similarity search, OpenAI Embeddings API, Vector DB (Pinecone), Tailwind, Full-stack Type Safety

Use this template:

```
wasp new <project-name> -t embeddings
```

### Todo App w/ Typescript

A simple Todo App with Typescript and Full-stack Type Safety.

**Features:** Auth (username/password), Full-stack Type Safety

Use this template:

```
wasp new <project-name> -t todo-ts
```

### AI Generated Starter 🤖

Using the same tech as used on https://usemage.ai/, Wasp generates your custom starter template based on your
project description. It will automatically generate your data model, auth, queries, actions and React pages.

_You will need to provide your own OpenAI API key to be able to use this template._

**Features:** Generated using OpenAI's GPT models, Auth (username/password), Queries, Actions, Pages, Full-stack Type Safety
