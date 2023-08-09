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
[2] saas
[3] todo-ts
▸ 1

🐝 --- Creating your project from the basic template... ---------------------------

Created new Wasp app in ./MyFirstProject directory!
To run it, do:

    cd MyFirstProject
    wasp start
```

## Available Templates

When you have a good idea for a new product, you don't want to waste your time on setting up common things like authentication, database, etc. That's why we created a few starter templates to help you get started with Wasp.

### Vector Similarity Search Template

![Vector Similarity Search Template](/img/starter-templates/embeddings-client.png)

A template for generating embeddings and performing vector similarity search on your text data!

**Features:** w/ Embeddings & vector similarity search, OpenAI Embeddings API, Vector DB (Pinecone), Tailwind, Fullstack Type Safety

Use this template:

```
wasp new <project-name> -t embeddings
```

### SaaS Template

![SaaS Template](/img/starter-templates/gptsaastemplate.png)

A SaaS Template to get your profitable side project started quickly and easily!

**Features:** w/ Stripe Payments, OpenAI GPT API, Google Auth, SendGrid, Tailwind, & Cron Jobs

Use this template:

```
wasp new <project-name> -t saas
```

### Todo App w/ Typescript

A simple Todo App with Typescript and Fullstack Type Safety.

**Features:** Auth (username/password), Fullstack Type Safety

Use this template:

```
wasp new <project-name> -t todo-ts
```
