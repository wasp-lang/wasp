---
title: Deploying
---

:::danger
While you can deploy Wasp projects, Wasp is still in alpha and not yet production-ready.

It might have critical security issues or other types of issues, and therefore we don't recommend deploying to production yet.
:::

Right now, deploying of Wasp project is done by generating the code and then deploying generated code "manually", as explained below.

In the future, the plan is to have Wasp take care of it completely: you would declaratively define your deployment in .wasp and then just call `wasp deploy` ([github issue](https://github.com/wasp-lang/wasp/issues/169)).

# Generating deployable code
```
wasp build
```
generates deployable code for the whole app in the `.wasp/build/` directory. Next, we will deploy this code.

NOTE: You will not be able to build the app if you are using SQLite as a database (which is a default database) -> you will have to [switch to PostgreSQL](/docs/language/features#migrating-from-sqlite-to-postgresql).

# Deploying API server (backend)
In `.wasp/build/`, there is a `Dockerfile` describing an image for building the server.

To run server in production, deploy this docker image to your favorite hosting provider, ensure that env vars are correctly set, and that is it.

Below we will explain the required env vars and also provide detailed instructions for deploying to Heroku.

## Env vars

Server uses following environment variables, so you need to ensure they are set on your hosting provider:
- `PORT` -> number of port at which it will listen for requests (e.g. `3001`).
- `DATABASE_URL` -> url to the Postgres database that it should use (e.g. `postgresql://mydbuser:mypass@localhost:5432/nameofmydb`)
- `JWT_SECRET` -> you need this if you are using Wasp's `auth` feature. Set it to a random string (password), at least 32 characters long.

## Deploying to Heroku

Heroku is completely free under certain limits, so it is ideal for getting started with deploying a Wasp app.
You will need Heroku account, `heroku` CLI and `docker` CLI installed to follow these instructions.

Make sure you are logged in with `heroku` CLI. You can check if you are logged in with `heroku whoami`, and if you are not, you can log in with `heroku login`.

### Set up a Heroku app (only once per Wasp app)
Unless you already have a heroku app that you want to deploy to, let's create a new Heroku app:
```
heroku create <app-name>
```

Unless you have external Postgres database that you want to use, let's create new database on Heroku and attach it to our app:
```
heroku addons:create --app <app-name> heroku-postgresql:hobby-dev
```
Heroku will also set `DATABASE_URL` env var for us at this point. If you are using external database, you will have to set it yourself.

`PORT` env var will also be provided by Heroku, so the only thing left is to set `JWT_SECRET` env var:
```
heroku config:set --app <app-name> JWT_SECRET=<random_string_at_least_32_characters_long>
```

### Deploy to a Heroku app
Position yourself in `.wasp/build/` directory (reminder: which you created by running `wasp build` previously):
```
cd .wasp/build
```
assuming you were at the root of your Wasp project at that moment.

Log in to Heroku Container Registry:
```
heroku container:login
```

Build the docker image and push it to Heroku:
```
heroku container:push --app <app-name> web
```
App is still not deployed at this point.
This step might take some time, especially the very first time, since there are no cached docker layers.

Deploy the pushed image and restart the app:
```
heroku container:release --app <app-name> web
```
This is it, backend is deployed at `https://<app-name>.herokuapp.com`!

Additionally, you can check out the logs with:
```
heroku logs --tail --app <app-name>
```

### Note on pg-boss
If you wish to deploy an app using Jobs powered by pg-boss to Heroku, you need to set an additional environment variable called `PG_BOSS_NEW_OPTIONS` to `{"connectionString":"<REGULAR_HEROKU_DATABASE_URL>","ssl":{"rejectUnauthorized":false}}`. This is because pg-boss uses the `pg` extension, which does not seem to connect to Heroku over SSL by default, which Heroku requires. Additionally, Heroku uses a self-signed cert, so we must handle that as well.
- https://devcenter.heroku.com/articles/connecting-heroku-postgres#connecting-in-node-js

# Deploying web client (frontend)
Position yourself in `.wasp/build/web-app` directory (reminder: which you created by running `wasp build` previously):
```
cd .wasp/build/web-app
```
assuming you were at the root of your Wasp project at that moment.

Run
```
npm install && REACT_APP_API_URL=<url_to_wasp_backend> npm run build
```
where <url_to_wasp_backend> is url of the wasp backend that you previously deployed, e.g. `https://wasp-test.herokuapp.com`.

This will create `build/` directory, which you can deploy to any static hosting provider.
Check instructions below for deploying to Netlify.

## Deploying to Netlify

Netlify is a static hosting solution that is free for many use cases.
You will need Netlify account and `netlify` CLI installed to follow these instructions.

Make sure you are logged in with `netlify` CLI. You can check if you are logged in with `netlify status`, and if you are not, you can log in with `netlify login`.

While positioned in `.wasp/build/web-app/` directory, and after you have created `.wasp/build/web-app/build/` directory as per instructions above, run
```
netlify deploy
```
and that is it!
