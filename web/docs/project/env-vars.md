---
title: Env vars
---

**Environment variables** are a mean of configuring your project in regards to the current environment it is executing in, allowing it to have different behaviour in different environments (development, staging, production, ...).

For example, you will want your project to connect to the local development database running on your machine when developing, but in production you will want it to connect to the production database.
Or, in development you will want it to use test Stripe account, while in production it should use real Stripe account.

Some env vars are required by Wasp (e.g. connection to the database, or social provider keys if you are using social auth), while on the other hand you can also define your own env vars for whatever you find useful.

There are two main parts of any Wasp project we are configuring with env vars: client and server, so we differentiate between client env vars and server env vars.

## Client env vars

Client env vars are embedded into the client code when it is built and shipped, meaning they are public and readable by anybody, therefore you should **never store secrets in them** (e.g. secret api keys).

For Wasp to pick them up, client env vars need to be prefixed with `REACT_APP_`, e.g. `REACT_APP_SOME_VAR_NAME=...`.

You can read them from the client JS code like this:
```js
console.log(import.meta.env.REACT_APP_SOME_VAR_NAME);
```

Check below on how to define them.

## Server env vars

Unlike client env vars, in server env vars you can, and often will, store secret values (e.g. secret api keys), since they are are not publicly readable.

Also, they don't need any kind of special prefix, you can just define them like `SOME_VAR_NAME=...`.

You can read them from the server Node.js code like this:
```js
console.log(process.env.SOME_VAR_NAME);
```

Check below on how to define them.

## Defining env vars in development

During development, there are two ways to provide env vars to your Wasp project:
1. **Via .env files. (recommended)**
2. Via shell. (useful for overrides)

### Via .env (dotenv) files
This is a recommended method for providing env vars to your Wasp project during development.

In the root of your Wasp project you can create two distinct files:
 - `.env.server` for env vars that will be provided to the server.
 - `.env.client` for env vars that will be provided to the client.

These files are not to be commited to the version control, and they are already ignored by default in the `.gitignore` file that comes with the new Wasp project.

Variables are defined in these files in the form of `NAME=VALUE`, for example:
```sh title=".env.server"
DATABASE_URL=postgresql://localhost:5432
SOME_VAR_NAME=somevalue
```
```sh title=".env.client"
REACT_APP_SOME_VAR_NAME=somevalue
```

`dotenv` files are a popular method for storing configuration: to learn more about them in general, check out the [README of the lib we use for them](https://github.com/stackbuilders/dotenv-hs).

### Via shell
If you set env vars in the shell where you run your Wasp commands (e.g. `wasp start`), Wasp will pick those up, as you would expect.
 
This is normally done by setting env vars in the .profile or similar file, or by defining them at the start of the Wasp command (e.g. `SOME_VAR_NAME=SOMEVALUE wasp start`).
None of this is Wasp-specific behaviour, this is just how env vars can be set in the shell.

Defining env vars this way can be quite cumbersome, even for one project, and even harder to manage if you have multiple Wasp projects, which is why we don't recommend it as a default method for providing env vars to Wasp projects.
However, **it can be useful for overriding** specific env vars occassionaly, because **env vars set this way have precedence over env vars defined in .env files**.


## Defining env vars in production

TODO: explain how are then env vars used during production -> that for the server they are provided via wherever they are hosted, usually via some kind of secrets / config mechanism, while for the client, they are embedded into the code during the build step, when static files for it are being generated (npm run build after wasp build). Maybe nice diagram could work well here, explaining these concepts simply, or maybe it is all too simple, not sure.
I suggest we explain the basic concept of it here, shortly, as explained above, and then provide more details under the Deployment section (and we can link from here to it). Especially for Fly, we don't explain at all there how to set the client env vars, we should.
Here is an issue covering this: https://github.com/wasp-lang/wasp/issues/1351
