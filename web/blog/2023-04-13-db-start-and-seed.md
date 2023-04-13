---
title: 'Wasp steps up its database game with Fully Managed Dev DB & DB Seeding'
authors: [martinsos]
image: /img/db-start-and-seed/wasp-db-improvements.png
tags: [database, wasp, webdev, prisma]
---

import Link from '@docusaurus/Link';
import useBaseUrl from '@docusaurus/useBaseUrl';

import InBlogCta from './components/InBlogCta';
import WaspIntro from './_wasp-intro.md';
import ImgWithCaption from './components/ImgWithCaption';
import DiscordLink from './components/DiscordLink';

As a full-stack framework, Wasp doesn’t care “just” about frontend and backend, but it also covers the database!

It does this by allowing you to define Prisma data models in a Wasp file, connecting them to the relevant Wasp Operations, warning you if you need to do database migrations, deploying the database for you (if you choose so), … .

Since Wasp knows so much about your database, that puts us in a good position to keep finding ways to improve the developer experience regarding dealing with the database. For Wasp v0.10, we focused on:

1. Wasp running the dev database for you with no config needed → **Fully Managed Dev Database** 🚀
2. Wasp helping you to initialize the database with some data → **Db Seeding** 🌱

<ImgWithCaption
    caption="Wasp now has `wasp start db` and `wasp db seed`!"
    alt="strong wasp database"
    source="img/db-start-and-seed/wasp-db-improvements.png"
/>

<!--truncate-->

## Fully Managed Dev Database 🚀

You might have asked yourself:

> If Wasp already knows so much about my database, why do I need to bother running it on my own!?
> 

Ok, when you start a new Wasp project it is easy because you are using an SQLite database, but once you switch to Postgres, it falls onto you to take care of it: run it, provide its URL to Wasp via env var, handle multiple databases if you have multiple Wasp apps, … .

This can get tedious quickly, especially if you are visiting your Wasp project that you haven’t worked on for a bit and need to figure out again how to run the db, or you need to check out somebody else’s Wasp project and don’t have it all set up yet. It is something most of us are used to, especially with other frameworks, but still, we can do better at Wasp!

This is where `wasp start db` comes in!

<ImgWithCaption
    caption="wasp start db in action, running a posgtres dev db for you"
    alt="wasp start db running in terminal"
    source="img/db-start-and-seed/wasp-start-db-terminal.png"
/>

Now, all you need to do to run the development database, is run `wasp start db`, and Wasp will run it for you and will know how to connect to it during development.

No env var setting, no remembering how to run the db. The only requirement is that you have `Docker` installed on your machine. Data from your database will be persisted on the disk between the runs, and each Wasp app will have its own database assigned. 

Btw, you can still use a custom database that you ran on your own if you want, the same way it was done before in Wasp: by setting env var `DATABASE_URL`.

## Database seeding 🌱

**Database seeding** is a term for populating the database with some initial data.

Seeding is most commonly used for two following scenarios:

1. To put the development database into a state convenient for testing / playing with it.
2. To initialize the dev/staging/prod database with some essential data needed for it to be useful, for example, default currencies in a Currency table.

Wasp so far had no direct support for seeding, so you had to either come up with your own solution (e.g. script that connects to the db and executes some queries), or massage data manually via Prisma Studio (`wasp db studio`).

There is one big drawback to both of the approaches I mentioned above though: there is no easy way to reuse logic that you have already implemented in your Wasp app, especially Actions (e.g. `createTask`)! This is pretty bad, as it makes your seeding logic brittle.

This is where `wasp db seed` comes in! Now, Wasp allows you to write a JS/TS function, import any server logic (including Actions) into it as you wish, and then seed the database with it.

<ImgWithCaption
    caption="wasp db seed in action, initializing the db with dev data"
    alt="wasp db seed running in terminal"
    source="img/db-start-and-seed/wasp-db-seed-terminal.png"
/>

Registering seed functions in Wasp is easy:

```jsx
app MyApp {
  // ...
  db: {
    // ...
    seeds: [
      import { devSeedSimple } from "@server/dbSeeds.js",
      import { prodSeed } from "@server/dbSeeds.js"
    ]
  }
}
```

Example of a seed function from above, `devSeedSimple`:

```jsx
import { createTask } from './actions.js'

export const devSeedSimple = async (prismaClient) => {
  const user = await createUser(prismaClient, {
      username: "RiuTheDog",
      password: "bark1234"
  })

  await createTask(
    { description: "Chase the cat" },
    { user, entities: { Task: prismaClient.task } }
  )
}

async function createUser (prismaClient, data) {
  const { password, ...newUser } = await prismaClient.user.create({ data })
  return newUser
}
```

Finally, to run these seeds, you can either do:

- `wasp db seed`: If you have just one seed function, it will run it. If you have multiple, it will interactively ask you to choose one to run.
- `wasp db seed <seed-name>`: It will run the seed function with the specified name, where the name is the identifier you used in its `import` expression in the `app.db.seeds` list. Example: `wasp db seed devSeedSimple`.

We also added `wasp db reset` command (calls `prisma db reset` in the background) that cleans up the database for you (removes all data and tables and re-applies migrations), which is great to use in combination with `wasp db seed`, as a precursor.

## Plans for the future 🔮

- allow customization of managed dev database (Postgres plugins, custom Dockerfile, …)
- have Wasp run the managed dev database automatically whenever it needs it (instead of you having to run `wasp start db` manually)
- dynamically find a free port for managed dev database (right now it requires port 5432)
- provide utility functions to make writing seeding functions easier (e.g. functions for creating new users)
- right now seeding functions are defined as part of a Wasp server code → it might be interesting to separate them in a standalone “project” in the future, while still keeping their easy access to the server logic.
- do you have any ideas/suggestions? Let us know in our <DiscordLink />!
