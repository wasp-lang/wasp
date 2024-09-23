---
title: Migration from 0.11.X to 0.12.X
---

import { EmailPill, UsernameAndPasswordPill, GithubPill, GooglePill } from "../auth/Pills";

:::note Migrating to the latest version

To fully migrate from 0.11.X to the latest version of Wasp, you should first migrate to **0.12.X** and then continue going through the migration guides.

Make sure to read the [migration guide from 0.12.X to 0.13.X](./migrate-from-0-12-to-0-13.md) after you finish this one.

:::

## What's new in Wasp 0.12.0?

### New project structure

Here's a file tree of a fresh Wasp project created with the previous version of Wasp.
More precisely, this is what you'll get if you run `wasp new myProject` using Wasp 0.11.x:

```
.
├── .gitignore
├── main.wasp
├── src
│   ├── client
│   │   ├── Main.css
│   │   ├── MainPage.jsx
│   │   ├── react-app-env.d.ts
│   │   ├── tsconfig.json
│   │   └── waspLogo.png
│   ├── server
│   │   └── tsconfig.json
│   ├── shared
│   │   └── tsconfig.json
│   └── .waspignore
└── .wasproot
```

Compare that with the file tree of a fresh Wasp project created with Wasp
0.12.0. In other words, this is what you will get by running `wasp new myProject`
from this point onwards:

```
.
├── .gitignore
├── main.wasp
├── package.json
├── public
│   └── .gitkeep
├── src
│   ├── Main.css
│   ├── MainPage.jsx
│   ├── queries.ts
│   ├── vite-env.d.ts
│   ├── .waspignore
│   └── waspLogo.png
├── tsconfig.json
├── vite.config.ts
└── .wasproot

```

The main differences are:

- The server/client code separation is no longer necessary. You can now organize
  your code however you want, as long as it's inside the `src` directory.
- All external imports in your Wasp file must have paths starting with `@src` (e.g., `import foo from '@src/bar.js'`)
  where `@src` refers to the `src` directory in your project root. The paths can
  no longer start with `@server` or `@client`.
- Your project now features a top-level `public` dir. Wasp will publicly serve
  all the files it finds in this directory. Read more about it
  [here](../project/static-assets.md).

Our [Overview docs](../tutorial/02-project-structure.md) explain the new
structure in detail, while this page provides a [quick guide](#migrating-your-project-to-the-new-structure) for migrating existing
projects.

### New auth

In Wasp 0.11.X, authentication was based on the `User` model which the developer needed to set up properly and take care of the auth fields like `email` or `password`.

```wasp title="main.wasp"
app myApp {
  wasp: {
    version: "^0.11.0"
  },
  title: "My App",
  auth: {
    userEntity: User,
    externalAuthEntity: SocialLogin,
    methods: {
      gitHub: {}
    },
    onAuthFailedRedirectTo: "/login"
  },
}

entity User {=psl
  id                        Int           @id @default(autoincrement())
  // highlight-start
  username                  String        @unique
  password                  String
  externalAuthAssociations  SocialLogin[]
  // highlight-end
psl=}


// highlight-start
entity SocialLogin {=psl
  id          Int       @id @default(autoincrement())
  provider    String
  providerId  String
  user        User      @relation(fields: [userId], references: [id], onDelete: Cascade)
  userId      Int
  createdAt   DateTime  @default(now())
  @@unique([provider, providerId, userId])
psl=}
// highlight-end
```

From 0.12.X onwards, authentication is based on the auth models which are automatically set up by Wasp. You don't need to take care of the auth fields anymore.

The `User` model is now just a business logic model and you use it for storing the data that is relevant for your app.

```wasp title="main.wasp"
app myApp {
  wasp: {
    version: "^0.12.0"
  },
  title: "My App",
  auth: {
    userEntity: User,
    methods: {
      gitHub: {}
    },
    onAuthFailedRedirectTo: "/login"
  },
}

entity User {=psl
  id Int @id @default(autoincrement())
psl=}
```

:::caution Regression Note: Multiple Auth Identities per User

With our old auth implementation, if you were using both Google and email auth methods, your users could sign up with Google first and then, later on, reset their password and therefore also enable logging in with their email and password. This was the only way in which a single user could have multiple login methods at the same time (Google and email).

This is not possible anymore. **The new auth system doesn't support multiple login methods per user at the moment**. We do plan to add this soon though, with the introduction of the [account merging feature](https://github.com/wasp-lang/wasp/issues/954).

If you have any users that have both Google and email login credentials at the same time, you will have to pick only one of those for that user to keep when migrating them.

:::

:::caution Regression Note: `_waspCustomValidations` is deprecated

Auth field customization is no longer possible using the `_waspCustomValidations` on the `User` entity. This is a part of auth refactoring that we are doing to make it easier to customize auth. We will be adding more customization options in the future.

:::

You can read more about the new auth system in the [Accessing User Data](../auth/entities) section.

## How to Migrate?

These instructions are for migrating your app from Wasp `0.11.X` to Wasp `0.12.X`, meaning they will work for all minor releases that fit this pattern (e.g., the guide applies to `0.12.0`, `0.12.1`, ...).

The guide consists of two big steps:
1. Migrating your Wasp project to the new structure.
2. Migrating to the new auth.

If you get stuck at any point, don't hesitate to ask for help on [our Discord server](https://discord.gg/rzdnErX).

### Migrating Your Project to the New Structure

You can easily migrate your old Wasp project to the new structure by following a
series of steps. Assuming you have a project called `foo` inside the
directory `foo`, you should:

0. **Install the `0.12.x` version** of Wasp.
  ```bash
  curl -sSL https://get.wasp-lang.dev/installer.sh | sh -s -- -v 0.12.4
  ```
1. Make sure to **backup or save your project** before starting the procedure (e.g.,
   by committing it to source control or creating a copy).
2. **Position yourself in the terminal** in the directory that is a parent of your wasp project directory (so one level above: if you do `ls`, you should see your wasp project dir listed).
3. **Run the migration script** (replace `foo` at the end with the name of your Wasp project directory) and follow the instructions:
  ```
  npx wasp-migrate foo
  ```

<details>
  <summary>
    In case the migration script doesn't work well for you, you can do the same steps manually, as described here:
  </summary>
<div>

1. Rename your project's root directory to something like `foo_old`.
2. Create a new project by running `wasp new foo`.
3. Delete all files of `foo/src` except `vite-env.d.ts`.
4. If `foo_old/src/client/public` exists and contains any files, copy those files into
   `foo/public`.
5. Copy the contents of `foo_old/src` into `foo/src`.
   `foo/src` should now contain `vite-env.d.ts`, `.waspignore`, and three subdirectories (`server`, `client`, and `shared`).
   Don't change anything about this structure yet.
6. Delete redundant files and folders from `foo/src`:
   - `foo/src/.waspignore` - A new version of this file already exists at the top level.
   - `foo/src/client/vite-env.d.ts` - A new version of this file already exists at the top level.
   - `foo/src/client/tsconfig.json` - A new version of this file already exists at the top level.
   - `foo/src/server/tsconfig.json` - A new version of this file already exists at the top level.
   - `foo/src/shared/tsconfig.json` - A new version of this file already exists at the top level.
   - `foo/src/client/public` - You've moved all the files from this directory in step 5.
7. Update all the `@wasp` imports in your JS(X)/TS(X) source files in the `src/` dir.

   For this, we prepared a special script that will rewrite these imports automatically for you.

   Before doing this step, as the script will modify your JS(X)/TS(X) files in place, we advise committing
   all changes you have so far, so you can then both easily inspect the import rewrites that our
   script did (with `git diff`) and also revert them if something went wrong.

   To run the import-rewriting script, make sure you are in the root dir of your wasp project, and then run
   ```
   npx jscodeshift@0.15.1 -t https://raw.githubusercontent.com/wasp-lang/wasp-codemod/main/src/transforms/imports-from-0-11-to-0-12.ts --extensions=js,ts,jsx,tsx src/
   ```

   Then, check the changes it did, in case some kind of manual intervention is needed (in which case you should see TODO comments generated by the script).

   Alternatively, you can find all the mappings of old imports to the new ones in [this table](https://docs.google.com/spreadsheets/d/1QW-_16KRGTOaKXx9NYUtjk6m2TQ0nUMOA74hBthTH3g/edit#gid=1725669920) and use it to fix some/all of them manually.
8. Replace the Wasp file in `foo` (i.e., `main.wasp`) with the Wasp file from `foo_old`
9. Change the Wasp version field in your Wasp file (now residing in `foo`) to `"^0.12.0"`.
10. Correct external imports in your Wasp file (now residing in `foo`).
    imports. You can do this by running search-and-replace inside the file:

    - Change all occurrences of `@server` to `@src/server`
    - Change all occurrences of `@client` to `@src/client`

    For example, if you previously had something like:

    ```js
    page LoginPage {
      // highlight-next-line
      // This previously resolved to src/client/LoginPage.js
      // highlight-next-line
      component: import Login from "@client/LoginPage"
    }

    // ...

    query getTasks {
      // highlight-next-line
      // This previously resolved to src/server/queries.js
      // highlight-next-line
      fn: import { getTasks } from "@server/queries.js",
    }
    ```

    You should change it to:

    ```js
    page LoginPage {
      // highlight-next-line
      // This now resolves to src/client/LoginPage.js
      // highlight-next-line
      component: import Login from "@src/client/LoginPage"
    }

    // ...

    query getTasks {
      // highlight-next-line
      // This now resolves to src/server/queries.js
      // highlight-next-line
      fn: import { getTasks } from "@src/server/queries.js",
    }
    ```

    Do this for all external imports in your `.wasp` file. After you're done, there shouldn't be any occurrences of strings `"@server"` or `"@client"`

11. Take all the dependencies from `app.dependencies` declaration in
    `foo/main.wasp` and move them to `foo/package.json`. Make sure to remove the `app.dependencies` field from `foo/main.wasp`.

    For example, if `foo_old/main.wasp` had:

    ```css
    app Foo {
      // ...
      dependencies: [ ('redux', '^4.0.5'), ('reacjt-redux', '^7.1.3')];
    }
    ```

    Your `package.json` in `foo` should now list these dependencies (Wasp already generated most of the file, you just have to list additional dependencies).

    ```json
    {
      "name": "foo",
      "dependencies": {
        "wasp": "file:.wasp/out/sdk/wasp",
        "react": "^18.2.0",
        // highlight-next-line
        "redux": "^4.0.5",
        // highlight-next-line
        "reactjs-redux": "^7.1.3"
      },
      "devDependencies": {
        "typescript": "^5.1.0",
        "vite": "^4.3.9",
        "@types/react": "^18.0.37",
        "prisma": "4.16.2"
      }
    }
    ```

12. Copy all lines you might have added to `foo_old/.gitignore` into
    `foo/.gitignore`
13. Copy the rest of the top-level files and folders (all of them except for `.gitignore`, `main.wasp` and `src/`)
    in `foo_old/` into `foo/` (overwrite the existing files in `foo`).
14. Run `wasp clean` in `foo`.
15. Delete the `foo_old` directory.

</div>
</details>

That's it! You now have a properly structured Wasp 0.12.0 project in the `foo` directory.
Your app probably doesn't quite work yet due to some other changes in Wasp 0.12.0, but we'll get to that in the next sections.

### Migrating declaration names
Wasp 0.12.0 adds a casing constraints when naming Queries, Actions, Jobs, and Entities in the `main.wasp` file.

The following casing conventions have now become mandatory:
- Operation (i.e., Query and Action) names must begin with a lowercase letter: `query getTasks {...}`, `action createTask {...}`.
- Job names must begin with a lowercase letter: `job sendReport {...}`.
- Entity names must start with an uppercase letter: `entity Task {...}`.

### Migrating the Tailwind Setup

:::note
If you don't use Tailwind in your project, you can skip this section.
:::

There is a small change in how the `tailwind.config.cjs` needs to be defined in Wasp 0.12.0.

You'll need to wrap all your paths in the `content` field with the `resolveProjectPath` function. This makes sure that the paths are resolved correctly when generating your CSS.

Here's how you can do it:

<Tabs>
<TabItem value="before" label="Before">

```js title="tailwind.config.cjs"
/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    // highlight-next-line
    './src/**/*.{js,jsx,ts,tsx}',
  ],
  theme: {
    extend: {},
  },
  plugins: [],
}
```
</TabItem>

<TabItem value="after" label="After">

```js title="tailwind.config.cjs"
// highlight-next-line
const { resolveProjectPath } = require('wasp/dev')

/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    // highlight-next-line
    resolveProjectPath('./src/**/*.{js,jsx,ts,tsx}'),
  ],
  theme: {
    extend: {},
  },
  plugins: [],
}
```
</TabItem>
</Tabs>

### Default Server Dockerfile Changed
:::note
If you didn't customize your Dockerfile or had a custom build process for the Wasp server, you can skip this section.
:::

Between Wasp 0.11.X and 0.12.X, the Dockerfile that Wasp generates for you for deploying the server has changed. If you defined a custom Dockerfile in your project root dir or in any other way relied on its contents, you'll need to update it to incorporate the changes that Wasp 0.12.X made.

We suggest that you temporarily move your custom Dockerfile to a different location, then run `wasp start` to generate the new Dockerfile.
Check out the `.wasp/out/Dockerfile` to see the new Dockerfile and what changes you need to make. You'll probably need to copy some of the changes from the new Dockerfile to your custom one to make your app work with Wasp 0.12.X.

### Migrating to the New Auth
As shown in [the previous section](#new-auth), Wasp significantly changed how authentication works in version 0.12.0.
This section leads you through migrating your app from Wasp 0.11.X to Wasp 0.12.X.

Migrating your existing app to the new auth system is a two-step process:

1. Migrate to the new auth system
1. Clean up the old auth system

:::info Migrating a deployed app

While going through these steps, we will focus first on doing the changes locally (including your local development database).

Once we confirm everything works well locally, we will apply the same changes to the deployed app (including your production database).

**We'll put extra info for migrating a deployed app in a box like this one.**
:::

#### 1. Migrate to the New Auth System

You can follow these steps to migrate to the new auth system (assuming you already migrated the project structure to 0.12, as described [above](#migrating-your-project-to-the-new-structure)):

1. **Migrate `getUserFields` and/or `additionalSignupFields` in the `main.wasp` file to the new `userSignupFields` field.** 

  If you are not using them, you can skip this step.

  In Wasp 0.11.X, you could define a `getUserFieldsFn` to specify extra fields that would get saved to the `User` when using Google or GitHub to sign up.
    
  You could also define `additionalSignupFields` to specify extra fields for the Email or Username & Password signup.

  In 0.12.X, we unified these two concepts into the `userSignupFields` field.

  <details>
  <summary>Migration for <EmailPill /> and <UsernameAndPasswordPill /></summary>

    First, move the value of `auth.signup.additionalFields` to `auth.methods.{method}.userSignupFields` in the `main.wasp` file.
      
    `{method}` depends on the auth method you are using. For example, if you are using the email auth method, you should move the `auth.signup.additionalFields` to `auth.methods.email.userSignupFields`.
    
    To finish, update the JS/TS implementation to use the `defineUserSignupFields` from `wasp/server/auth` instead of `defineAdditionalSignupFields` from `@wasp/auth/index.js`.

    <Tabs>
    <TabItem value="before" label="Before">

    ```wasp title="main.wasp"
    app crudTesting {
      // ...
      auth: {
        userEntity: User,
        methods: {
          email: {},
        },
        onAuthFailedRedirectTo: "/login",
        // highlight-start
        signup: {
          additionalFields: import { fields } from "@server/auth/signup.js",
        },
        // highlight-end
      },
    }
    ```

    ```ts title="src/server/auth/signup.ts"
    // highlight-next-line
    import { defineAdditionalSignupFields } from '@wasp/auth/index.js'

    // highlight-next-line
    export const fields = defineAdditionalSignupFields({
      address: async (data) => {
        const address = data.address
        if (typeof address !== 'string') {
          throw new Error('Address is required')
        }
        if (address.length < 5) {
          throw new Error('Address must be at least 5 characters long')
        }
        return address
      },
    })
    ```

    </TabItem>

    <TabItem value="after" label="After">

    ```wasp title="main.wasp"
    app crudTesting {
      // ...
      auth: {
        userEntity: User,
        methods: {
          email: {
            // highlight-next-line
            userSignupFields: import { fields } from "@src/server/auth/signup.js",
          },
        },
        onAuthFailedRedirectTo: "/login",
      },
    }
    ```

    ```ts title="src/server/auth/signup.ts"
    // highlight-next-line
    import { defineUserSignupFields } from 'wasp/server/auth'

    // highlight-next-line
    export const fields = defineUserSignupFields({
      address: async (data) => {
        const address = data.address;
        if (typeof address !== 'string') {
          throw new Error('Address is required');
        }
        if (address.length < 5) {
          throw new Error('Address must be at least 5 characters long');
        }
        return address;
      },
    })
    ```

    Read more about the `userSignupFields` function [here](/auth/overview.md#1-defining-extra-fields).

    </TabItem>
    </Tabs>

  </details>

  <details>
  <summary>Migration for <GithubPill /> and <GooglePill /></summary>

    First, move the value of `auth.methods.{method}.getUserFieldsFn` to `auth.methods.{method}.userSignupFields` in the `main.wasp` file.
      
    `{method}` depends on the auth method you are using. For example, if you are using Google auth, you should move the `auth.methods.google.getUserFieldsFn` to `auth.methods.google.userSignupFields`.

    To finish, update the JS/TS implementation to use the `defineUserSignupFields` from `wasp/server/auth` and modify the code to return the fields in the format that `defineUserSignupFields` expects.

    <Tabs>
    <TabItem value="before" label="Before">

    ```wasp title="main.wasp"
    app crudTesting {
      // ...
      auth: {
        userEntity: User,
        methods: {
          google: {
            // highlight-next-line
            getUserFieldsFn: import { getUserFields } from "@server/auth/google.js"
          },
        },
        onAuthFailedRedirectTo: "/login",
      },
    }
    ```

    ```ts title="src/server/auth/google.ts"
    // highlight-next-line
    import type { GetUserFieldsFn } from '@wasp/types'

    // highlight-start
    export const getUserFields: GetUserFieldsFn = async (_context, args) => {
      const displayName = args.profile.displayName
      return { displayName }
    }
    // highlight-end
    ```

    </TabItem>

    <TabItem value="after" label="After">

    ```wasp title="main.wasp"
    app crudTesting {
      // ...
      auth: {
        userEntity: User,
        methods: {
          google: {
            // highlight-next-line
            userSignupFields: import { fields } from "@src/server/auth/google.js",
          },
        },
        onAuthFailedRedirectTo: "/login",
      },
    }
    ```

    ```ts title="src/server/auth/signup.ts"
    // highlight-next-line
    import { defineUserSignupFields } from 'wasp/server/auth'

    // highlight-start
    export const fields = defineUserSignupFields({
      displayName: async (data) => {
        const profile: any = data.profile;
        if (!profile?.displayName) { throw new Error('Display name is not available'); }
        return profile.displayName;
      },
    })
    // highlight-end
    ```

    If you want to properly type the `profile` object, we recommend you use a validation library like Zod to define the shape of the `profile` object.
    
    Read more about this and the `defineUserSignupFields` function in the [Auth Overview - Defining Extra Fields](../auth/overview.md#1-defining-extra-fields) section.

    </TabItem>
    </Tabs>

  </details>

1. **Remove the `auth.methods.email.allowUnverifiedLogin` field** from your `main.wasp` file.

  In Wasp 0.12.X we removed the `auth.methods.email.allowUnverifiedLogin` field to make our Email auth implementation easier to reason about. If you were using it, you should remove it from your `main.wasp` file.

1. Ensure your **local development database is running**.
1. **Do the schema migration** (create the new auth tables in the database) by running:
   ```bash
   wasp db migrate-dev
   ```
You should see the new `Auth`, `AuthIdentity` and `Session` tables in your database. You can use the `wasp db studio` command to open the database in a GUI and verify the tables are there. At the moment, they will be empty.
1. **Do the data migration** (move existing users from the old auth system to the new one by filling the new auth tables in the database with their data):

   1. **Implement your data migration function(s)** in e.g. `src/migrateToNewAuth.ts`.

      Below we prepared [examples of migration functions](#example-data-migration-functions) for each of the auth methods, for you to use as a starting point.
      They should be fine to use as-is, meaning you can just copy them and they are likely to work out of the box for typical use cases, but you can also modify them for your needs.

      We recommend you create one function per each auth method that you use in your app.

   1. **Define custom API endpoints for each migration function** you implemented.

      With each data migration function below, we provided a relevant `api` declaration that you should add to your `main.wasp` file.

   1. **Run the data migration function(s)** on the local development database by calling the API endpoints you defined in the previous step.

      You can call the endpoint by visiting the URL in your browser, or by using a tool like `curl` or Postman. 
      
      For example, if you defined the API endpoint at `/migrate-username-and-password`, you can call it by visiting `http://localhost:3001/migrate-username-and-password` in your browser.

      This should be it, you can now run `wasp db studio` again and verify that there is now relevant data in the new auth tables (`Auth` and `AuthIdentity`; `Session` should still be empty for now).

1. **Verify that the basic auth functionality works** by running `wasp start` and successfully signing up / logging in with each of the auth methods.
1. **Update your JS/TS code** to work correctly with the new auth.

  You might want to use the new auth helper functions to get the `email` or `username` from a user object. For example, `user.username` might not work anymore for you, since the `username` obtained by the Username & Password auth method isn't stored on the `User` entity anymore (unless you are explicitly storing something into `user.username`, e.g. via `userSignupFields` for a social auth method like Github). Same goes for `email` from Email auth method. 
  
 Instead, you can now use `getUsername(user)` to get the username obtained from Username & Password auth method, or `getEmail(user)` to get the email obtained from Email auth method.
 
 Read more about the helpers in the [Accessing User Data](../auth/entities#accessing-the-auth-fields) section.
 
1. Finally, **check that your app now fully works as it worked before**. If all the above steps were done correctly, everything should be working now.

    :::info Migrating a deployed app

    After successfully performing migration locally so far, and verifying that your app works as expected, it is time to also migrate our deployed app.

    Before migrating your production (deployed) app, we advise you to back up your production database in case something goes wrong. Also, besides testing it in development, it's good to test the migration in a staging environment if you have one.

    We will perform the production migration in 2 steps:
    - Deploying the new code to production (client and server).
    - Migrating the production database data.

    ---

    Between these two steps, so after successfully deploying the new code to production and before migrating the production database data, your app will not be working completely: new users will be able to sign up, but existing users won't be able to log in, and already logged in users will be logged out. Once you do the second step, migrating the production database data, it will all be back to normal. You will likely want to keep the time between the two steps as short as you can.

    ---

    - **First step: deploy the new code** (client and server), either via `wasp deploy` (i.e. `wasp deploy fly deploy`) or manually.

      Check our [Deployment docs](advanced/deployment/overview.md) for more details.

    - **Second step: run the data migration functions** on the production database.

      You can do this by calling the API endpoints you defined in the previous step, just like you did locally. You can call the endpoint by visiting the URL in your browser, or by using a tool like `curl` or Postman. 

      For example, if you defined the API endpoint at `/migrate-username-and-password`, you can call it by visiting `https://your-server-url.com/migrate-username-and-password` in your browser.

    Your deployed app should be working normally now, with the new auth system.
    :::


#### 2. Cleanup the Old Auth System

Your app should be working correctly and using new auth, but to finish the migration, we need to clean up the old auth system:

1. In `main.wasp` file, **delete auth-related fields from the `User` entity**, since with 0.12 they got moved to the internal Wasp entity `AuthIdentity`.

    - This means any fields that were required by Wasp for authentication, like `email`, `password`, `isEmailVerified`, `emailVerificationSentAt`, `passwordResetSentAt`, `username`, etc.
    - There are situations in which you might want to keep some of them, e.g. `email` and/or `username`, if they are still relevant for you due to your custom logic (e.g. you are populating them with `userSignupFields` upon social signup in order to have this info easily available on the `User` entity). Note that they won't be used by Wasp Auth anymore, they are here just for your business logic.

1. In `main.wasp` file, **remove the `externalAuthEntity` field from the `app.auth`** and also **remove the whole `SocialLogin` entity** if you used Google or GitHub auth.
1. **Delete the data migration function(s)** you implemented earlier (e.g. in `src/migrateToNewAuth.ts`) and also the corresponding API endpoints from the `main.wasp` file.
1. **Run `wasp db migrate-dev`** again to apply these changes and remove the redundant fields from the database.

:::info Migrating a deployed app

  After doing the steps above successfully locally and making sure everything is working, it is time to push these changes to the deployed app again.

  _Deploy the app again_, either via `wasp deploy` or manually. Check our [Deployment docs](../advanced/deployment/overview.md) for more details.

  The database migrations will automatically run on successful deployment of the server and delete the now redundant auth-related `User` columns from the database.

  Your app is now fully migrated to the new auth system.

:::

### Next Steps

If you made it this far, you've completed all the necessary steps to get your
Wasp app working with Wasp 0.12.x. Nice work!

Finally, since Wasp no longer requires you to separate your client source files
(previously in `src/client`) from server source files (previously in
`src/server`), you are now free to reorganize your project however you think is best,
as long as you keep all the source files in the `src/` directory.

This section is optional, but if you didn't like the server/client
separation, now's the perfect time to change it.

For example, if your `src` dir looked like this:

```
src
│
├── client
│   ├── Dashboard.tsx
│   ├── Login.tsx
│   ├── MainPage.tsx
│   ├── Register.tsx
│   ├── Task.css
│   ├── TaskLisk.tsx
│   ├── Task.tsx
│   └── User.tsx
├── server
│   ├── taskActions.ts
│   ├── taskQueries.ts
│   ├── userActions.ts
│   └── userQueries.ts
└── shared
    └── utils.ts
```

you can now change it to a feature-based structure (which we recommend for any project that is not very small):

```
src
│
├── task
│   ├── actions.ts    -- former taskActions.ts
│   ├── queries.ts    -- former taskQueries.ts
│   ├── Task.css
│   ├── TaskLisk.tsx
│   └── Task.tsx
├── user
│   ├── actions.ts    -- former userActions.ts
│   ├── Dashboard.tsx
│   ├── Login.tsx
│   ├── queries.ts    -- former userQueries.ts
│   ├── Register.tsx
│   └── User.tsx
├── MainPage.tsx
└── utils.ts
```


## Appendix

### Example Data Migration Functions

The migration functions provided below are written with the typical use cases in mind and you can use them as-is. If your setup requires additional logic, you can use them as a good starting point and modify them to your needs.

Note that all of the functions below are written to be idempotent, meaning that running a function multiple times can't hurt. This allows executing a function again in case only a part of the previous execution succeeded and also means that accidentally running it one time too much won't have any negative effects. **We recommend you keep your data migration functions idempotent**.

#### Username & Password

To successfully migrate the users using the Username & Password auth method, you will need to do two things:
1. Migrate the user data

  <details>
  <summary>Username & Password data migration function</summary>

  ```wasp title="main.wasp"
  api migrateUsernameAndPassword {
    httpRoute: (GET, "/migrate-username-and-password"),
    fn: import { migrateUsernameAndPasswordHandler } from "@src/migrateToNewAuth",
    entities: []
  }
  ```

  ```ts title="src/migrateToNewAuth.ts"
  import { prisma } from "wasp/server";
  import { type ProviderName, type UsernameProviderData } from "wasp/server/auth";
  import { MigrateUsernameAndPassword } from "wasp/server/api";

  export const migrateUsernameAndPasswordHandler: MigrateUsernameAndPassword =
    async (_req, res) => {
      const result = await migrateUsernameAuth();

      res.status(200).json({ message: "Migrated users to the new auth", result });
    };

  async function migrateUsernameAuth(): Promise<{
    numUsersAlreadyMigrated: number;
    numUsersNotUsingThisAuthMethod: number;
    numUsersMigratedSuccessfully: number;
  }> {
    const users = await prisma.user.findMany({
      include: {
        auth: true,
      },
    });

    const result = {
      numUsersAlreadyMigrated: 0,
      numUsersNotUsingThisAuthMethod: 0,
      numUsersMigratedSuccessfully: 0,
    };

    for (const user of users) {
      if (user.auth) {
        result.numUsersAlreadyMigrated++;
        console.log("Skipping user (already migrated) with id:", user.id);
        continue;
      }

      if (!user.username || !user.password) {
        result.numUsersNotUsingThisAuthMethod++;
        console.log("Skipping user (not using username auth) with id:", user.id);
        continue;
      }

      const providerData: UsernameProviderData = {
        hashedPassword: user.password,
      };
      const providerName: ProviderName = "username";

      await prisma.auth.create({
        data: {
          identities: {
            create: {
              providerName,
              providerUserId: user.username.toLowerCase(),
              providerData: JSON.stringify(providerData),
            },
          },
          user: {
            connect: {
              id: user.id,
            },
          },
        },
      });
      result.numUsersMigratedSuccessfully++;
    }

    return result;
  }
  ```

  </details>

2. Provide a way for users to migrate their password 

    There is a **breaking change between the old and the new auth in the way the password is hashed**. This means that users will need to migrate their password after the migration, as the old password will no longer work.

    Since the only way users using username and password as a login method can verify their identity is by providing both their username and password (there is no email or any other info, unless you asked for it and stored it explicitly), we need to provide them a way to **exchange their old password for a new password**. One way to handle this is to inform them about the need to migrate their password (on the login page) and provide a custom page to migrate the password. 

<details>
<summary>
  Steps to create a custom page for migrating the password
</summary>

  1. You will need to install the `secure-password` and `sodium-native` packages to use the old hashing algorithm:

  ```bash
  npm install secure-password@4.0.0 sodium-native@3.3.0 --save-exact
  ```

  Make sure to save the exact versions of the packages.

  2. Then you'll need to create a new page in your app where users can migrate their password. You can use the following code as a starting point:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
route MigratePasswordRoute { path: "/migrate-password", to: MigratePassword }
page MigratePassword {
  component: import { MigratePasswordPage } from "@src/pages/MigratePassword"
}
```

```jsx title="src/pages/MigratePassword.jsx"
import {
  FormItemGroup,
  FormLabel,
  FormInput,
  FormError,
} from "wasp/client/auth";
import { useForm } from "react-hook-form";
import { migratePassword } from "wasp/client/operations";
import { useState } from "react";

export function MigratePasswordPage() {
  const [successMessage, setSuccessMessage] = useState(null);
  const [errorMessage, setErrorMessage] = useState(null);
  const form = useForm();

  const onSubmit = form.handleSubmit(async (data) => {
    try {
      const result = await migratePassword(data);
      setSuccessMessage(result.message);
    } catch (e) {
      console.error(e);
      if (e instanceof Error) {
        setErrorMessage(e.message);
      }
    }
  });

  return (
    <div style={{
      maxWidth: "400px",
      margin: "auto",
    }}>
      <h1>Migrate your password</h1>
      <p>
        If you have an account on the old version of the website, you can
        migrate your password to the new version.
      </p>
      {successMessage && <div>{successMessage}</div>}
      {errorMessage && <FormError>{errorMessage}</FormError>}
      <form onSubmit={onSubmit}>
        <FormItemGroup>
          <FormLabel>Username</FormLabel>
          <FormInput
            {...form.register("username", {
              required: "Username is required",
            })}
          />
          <FormError>{form.formState.errors.username?.message}</FormError>
        </FormItemGroup>
        <FormItemGroup>
          <FormLabel>Password</FormLabel>
          <FormInput
            {...form.register("password", {
              required: "Password is required",
            })}
            type="password"
          />
          <FormError>{form.formState.errors.password?.message}</FormError>
        </FormItemGroup>
        <button type="submit">Migrate password</button>
      </form>
    </div>
  );
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
route MigratePasswordRoute { path: "/migrate-password", to: MigratePassword }
page MigratePassword {
  component: import { MigratePasswordPage } from "@src/pages/MigratePassword"
}
```

```tsx title="src/pages/MigratePassword.tsx"
import {
  FormItemGroup,
  FormLabel,
  FormInput,
  FormError,
} from "wasp/client/auth";
import { useForm } from "react-hook-form";
import { migratePassword } from "wasp/client/operations";
import { useState } from "react";

export function MigratePasswordPage() {
  const [successMessage, setSuccessMessage] = useState<string | null>(null);
  const [errorMessage, setErrorMessage] = useState<string | null>(null);
  const form = useForm<{
    username: string;
    password: string;
  }>();

  const onSubmit = form.handleSubmit(async (data) => {
    try {
      const result = await migratePassword(data);
      setSuccessMessage(result.message);
    } catch (e: unknown) {
      console.error(e);
      if (e instanceof Error) {
        setErrorMessage(e.message);
      }
    }
  });

  return (
    <div style={{
      maxWidth: "400px",
      margin: "auto",
    }}>
      <h1>Migrate your password</h1>
      <p>
        If you have an account on the old version of the website, you can
        migrate your password to the new version.
      </p>
      {successMessage && <div>{successMessage}</div>}
      {errorMessage && <FormError>{errorMessage}</FormError>}
      <form onSubmit={onSubmit}>
        <FormItemGroup>
          <FormLabel>Username</FormLabel>
          <FormInput
            {...form.register("username", {
              required: "Username is required",
            })}
          />
          <FormError>{form.formState.errors.username?.message}</FormError>
        </FormItemGroup>
        <FormItemGroup>
          <FormLabel>Password</FormLabel>
          <FormInput
            {...form.register("password", {
              required: "Password is required",
            })}
            type="password"
          />
          <FormError>{form.formState.errors.password?.message}</FormError>
        </FormItemGroup>
        <button type="submit">Migrate password</button>
      </form>
    </div>
  );
}
```

</TabItem>
</Tabs>

  3. Finally, you will need to create a new operation in your app to handle the password migration. You can use the following code as a starting point:


<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
action migratePassword {
  fn: import { migratePassword } from "@src/auth",
  entities: []
}
```

```js title="src/auth.js"
import SecurePassword from "secure-password";
import { HttpError } from "wasp/server";
import {
  createProviderId,
  deserializeAndSanitizeProviderData,
  findAuthIdentity,
  updateAuthIdentityProviderData,
} from "wasp/server/auth";

export const migratePassword = async ({ password, username }, _context) => {
  const providerId = createProviderId("username", username);
  const authIdentity = await findAuthIdentity(providerId);

  if (!authIdentity) {
    throw new HttpError(400, "Something went wrong");
  }

  const providerData = deserializeAndSanitizeProviderData(
    authIdentity.providerData
  );

  try {
    const SP = new SecurePassword();

    // This will verify the password using the old algorithm
    const result = await SP.verify(
      Buffer.from(password),
      Buffer.from(providerData.hashedPassword, "base64")
    );

    if (result !== SecurePassword.VALID) {
      throw new HttpError(400, "Something went wrong");
    }

    // This will hash the password using the new algorithm and update the
    // provider data in the database.
    await updateAuthIdentityProviderData(providerId, providerData, {
      hashedPassword: password,
    });
  } catch (e) {
    throw new HttpError(400, "Something went wrong");
  }

  return {
    message: "Password migrated successfully.",
  };
};
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
action migratePassword {
  fn: import { migratePassword } from "@src/auth",
  entities: []
}
```

```ts title="src/auth.ts"
import SecurePassword from "secure-password";
import { HttpError } from "wasp/server";
import {
  createProviderId,
  deserializeAndSanitizeProviderData,
  findAuthIdentity,
  updateAuthIdentityProviderData,
} from "wasp/server/auth";
import { MigratePassword } from "wasp/server/operations";

type MigratePasswordInput = {
  username: string;
  password: string;
};
type MigratePasswordOutput = {
  message: string;
};

export const migratePassword: MigratePassword<
  MigratePasswordInput,
  MigratePasswordOutput
> = async ({ password, username }, _context) => {
  const providerId = createProviderId("username", username);
  const authIdentity = await findAuthIdentity(providerId);

  if (!authIdentity) {
    throw new HttpError(400, "Something went wrong");
  }

  const providerData = deserializeAndSanitizeProviderData<"username">(
    authIdentity.providerData
  );

  try {
    const SP = new SecurePassword();

    // This will verify the password using the old algorithm
    const result = await SP.verify(
      Buffer.from(password),
      Buffer.from(providerData.hashedPassword, "base64")
    );

    if (result !== SecurePassword.VALID) {
      throw new HttpError(400, "Something went wrong");
    }

    // This will hash the password using the new algorithm and update the
    // provider data in the database.
    await updateAuthIdentityProviderData<"username">(providerId, providerData, {
      hashedPassword: password,
    });
  } catch (e) {
    throw new HttpError(400, "Something went wrong");
  }

  return {
    message: "Password migrated successfully.",
  };
};
```

</TabItem>
</Tabs>

</details>


#### Email

To successfully migrate the users using the Email auth method, you will need to do two things:
1. Migrate the user data

  <details>
  <summary>Email data migration function</summary>

  ```wasp title="main.wasp"
  api migrateEmail {
    httpRoute: (GET, "/migrate-email"),
    fn: import { migrateEmailHandler } from "@src/migrateToNewAuth",
    entities: []
  }
  ```

  ```ts title="src/migrateToNewAuth.ts"
  import { prisma } from "wasp/server";
  import { type ProviderName, type EmailProviderData } from "wasp/server/auth";
  import { MigrateEmail } from "wasp/server/api";

  export const migrateEmailHandler: MigrateEmail =
    async (_req, res) => {
      const result = await migrateEmailAuth();

      res.status(200).json({ message: "Migrated users to the new auth", result });
    };

  async function migrateEmailAuth(): Promise<{
    numUsersAlreadyMigrated: number;
    numUsersNotUsingThisAuthMethod: number;
    numUsersMigratedSuccessfully: number;
  }> {
    const users = await prisma.user.findMany({
      include: {
        auth: true,
      },
    });

    const result = {
      numUsersAlreadyMigrated: 0,
      numUsersNotUsingThisAuthMethod: 0,
      numUsersMigratedSuccessfully: 0,
    };

    for (const user of users) {
      if (user.auth) {
        result.numUsersAlreadyMigrated++;
        console.log("Skipping user (already migrated) with id:", user.id);
        continue;
      }

      if (!user.email || !user.password) {
        result.numUsersNotUsingThisAuthMethod++;
        console.log("Skipping user (not using email auth) with id:", user.id);
        continue;
      }

      const providerData: EmailProviderData = {
        isEmailVerified: user.isEmailVerified,
        emailVerificationSentAt:
          user.emailVerificationSentAt?.toISOString() ?? null,
        passwordResetSentAt: user.passwordResetSentAt?.toISOString() ?? null,
        hashedPassword: user.password,
      };
      const providerName: ProviderName = "email";

      await prisma.auth.create({
        data: {
          identities: {
            create: {
              providerName,
              providerUserId: user.email,
              providerData: JSON.stringify(providerData),
            },
          },
          user: {
            connect: {
              id: user.id,
            },
          },
        },
      });
      result.numUsersMigratedSuccessfully++;
    }

    return result;
  }
  ```

  </details>

2. Ask the users to reset their password

  There is a **breaking change between the old and the new auth in the way the password is hashed**. This means that users will need to reset their password after the migration, as the old password will no longer work.

  It would be best to notify your users about this change and put a notice on your login page to **request a password reset**.

#### Google & GitHub

<details>
<summary>Google & GitHub data migration functions</summary>

```wasp title="main.wasp"
api migrateGoogle {
  httpRoute: (GET, "/migrate-google"),
  fn: import { migrateGoogleHandler } from "@src/migrateToNewAuth",
  entities: []
}

api migrateGithub {
  httpRoute: (GET, "/migrate-github"),
  fn: import { migrateGithubHandler } from "@src/migrateToNewAuth",
  entities: []
}
```

```ts title="src/migrateToNewAuth.ts"
import { prisma } from "wasp/server";
import { MigrateGoogle, MigrateGithub } from "wasp/server/api";

export const migrateGoogleHandler: MigrateGoogle =
  async (_req, res) => {
    const result = await createSocialLoginMigration("google");

    res.status(200).json({ message: "Migrated users to the new auth", result });
  };

export const migrateGithubHandler: MigrateGithub =
  async (_req, res) => {
    const result = await createSocialLoginMigration("github");

    res.status(200).json({ message: "Migrated users to the new auth", result });
  };

async function createSocialLoginMigration(
  providerName: "google" | "github"
): Promise<{
  numUsersAlreadyMigrated: number;
  numUsersNotUsingThisAuthMethod: number;
  numUsersMigratedSuccessfully: number;
}> {
  const users = await prisma.user.findMany({
    include: {
      auth: true,
      externalAuthAssociations: true,
    },
  });

  const result = {
    numUsersAlreadyMigrated: 0,
    numUsersNotUsingThisAuthMethod: 0,
    numUsersMigratedSuccessfully: 0,
  };

  for (const user of users) {
    if (user.auth) {
      result.numUsersAlreadyMigrated++;
      console.log("Skipping user (already migrated) with id:", user.id);
      continue;
    }

    const provider = user.externalAuthAssociations.find(
      (provider) => provider.provider === providerName
    );

    if (!provider) {
      result.numUsersNotUsingThisAuthMethod++;
      console.log(`Skipping user (not using ${providerName} auth) with id:`, user.id);
      continue;
    }

    await prisma.auth.create({
      data: {
        identities: {
          create: {
            providerName,
            providerUserId: provider.providerId,
            providerData: JSON.stringify({}),
          },
        },
        user: {
          connect: {
            id: user.id,
          },
        },
      },
    });
    result.numUsersMigratedSuccessfully++;
  }

  return result;
}
```

</details>
