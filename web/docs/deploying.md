---
title: Deploying
---

:::danger
While you can deploy Wasp projects, Wasp is still in alpha and not yet production-ready.

It might have critical security issues or other types of issues, and therefore we don't recommend deploying to production yet.
:::

Right now, deploying of Wasp project is done by generating the code and then deploying generated code "manually", as explained below.

In the future, the plan is to have Wasp take care of it completely: you would declaratively define your deployment in .wasp and then just call `wasp deploy` ([github issue](https://github.com/wasp-lang/wasp/issues/169)).

## Generating deployable code
```
wasp build
```
generates deployable code for the whole app in the `.wasp/build/` directory. Next, we will deploy this code.

NOTE: You will not be able to build the app if you are using SQLite as a database (which is a default database) -> you will have to [switch to PostgreSQL](/docs/language/features#migrating-from-sqlite-to-postgresql).

## Deploying API server (backend)
In `.wasp/build/`, there is a `Dockerfile` describing an image for building the server.

To run server in production, deploy this docker image to your favorite hosting provider, ensure that env vars are correctly set, and that is it.

Below we will explain the required env vars and also provide detailed instructions for deploying to Fly.io or Heroku.

### Env vars

Server uses following environment variables, so you need to ensure they are set on your hosting provider:
- `PORT` -> The port number at which it will listen for requests (e.g. `3001`).
- `DATABASE_URL` -> The URL of the Postgres database it should use (e.g. `postgresql://mydbuser:mypass@localhost:5432/nameofmydb`).
- `WASP_WEB_CLIENT_URL` -> The URL of where the frontend app is running (e.g. `https://<app-name>.netlify.app`), which is necessary for CORS.
- `JWT_SECRET` -> You need this if you are using Wasp's `auth` feature. Set it to a random string (password), at least 32 characters long.

### Deploying to Fly.io (free, recommended)

Fly.io offers a variety of free services that are perfect for deploying your first Wasp app! You will need a Fly.io account and the [`flyctl` CLI](https://fly.io/docs/hands-on/install-flyctl/).

:::note
Fly.io offers support for both locally built Docker containers and remotely built ones. However, for simplicity and reproducability, we will force the use of a remote Fly.io builder.

Additionally, `fly` is a symlink for `flyctl` on most systems and they can be used interchangeably.
:::

Make sure you are logged in with `flyctl` CLI. You can check if you are logged in with `flyctl auth whoami`, and if you are not, you can log in with `flyctl auth login`.

#### Set up a Fly.io app (only once per Wasp app)
Unless you already have a Fly.io app that you want to deploy to, let's create a new Fly.io app. Position yourself in .wasp/build/ directory (reminder: which you created by running `wasp build` previously):

```bash
cd .wasp/build
```

Now from within the `build` directory, run the launch command to set up a new app and create a `fly.toml` file:

```bash
flyctl launch --remote-only
```

This will ask a series of questions, including what region to deploy in and if you would like a database.
- Say **yes to "Would you like to set up a Postgresql database now?", and select Development**, and Fly.io will set a `DATABASE_URL` for you.
- Say **no to "Would you like to deploy now?"**, as well as any additional questions. We still need to set a few environment variables.

:::note 
If your attempts to initiate a new app fail for whatever reason, then you can run `flyctl apps destroy <app-name>` before trying again.

<details>
  <summary>
    What does it look like when your DB is deployed correctly?
  </summary>
  <div>
    <p>When your DB is deployed correctly, you will be able to view it in the <a href="https://fly.io/dashboard">Fly.io dashboard</a>:</p>
    <img width="662" alt="image" src="https://user-images.githubusercontent.com/70215737/201068630-d100db2c-ade5-4874-a29f-6e1890dba2fc.png" />
  </div>
</details>
:::
  
Next, let's copy the `fly.toml` file up to our Wasp project dir for safekeeping.
```bash
cp fly.toml ../../
```

Next, let's add a few more environment variables:
```bash
flyctl secrets set PORT=8080
flyctl secrets set JWT_SECRET=<random_string_at_least_32_characters_long>
flyctl secrets set WASP_WEB_CLIENT_URL=<url_of_where_frontend_will_be_deployed>
```

NOTE: If you do not know what your frontend URL is yet, don't worry. You can set `WASP_WEB_CLIENT_URL` after you deploy your frontend.

If you want to make sure you've added your secrets correctly, run `flyctl secrets list` in the terminal. Note that you will see hashed versions of your secrets to protect your sensitive data.

#### Deploy to a Fly.io app
While still in the .wasp/build/ directory, run:

```bash
flyctl deploy --remote-only --config ../../fly.toml
```

This will build and deploy the backend of your Wasp app on Fly.io to `https://<app-name>.fly.dev`! 🤘🎸

Now, if you haven't, you can deploy your frontend -- [we suggest using Netlify](#deploying-web-client-frontend) for this -- and add the client url by running `flyctl secrets set WASP_WEB_CLIENT_URL=<url_of_deployed_frontend>`

Additionally, some useful commands include:

```bash
flyctl logs
flyctl secrets list
flyctl ssh console
```

#### Redeploying after Wasp builds
When you rebuild your Wasp app (with `wasp build`), it will remove your .wasp/build/ directory. In there, you may have a `fly.toml` from any prior Fly.io deployments. While we will improve this process in the future, in the meantime, you have a few options:
1. Copy the `fly.toml` file to a versioned directory, like your Wasp project dir. From there, you can reference it in `flyctl deploy --config <path>` commands, like above.
1. Backup the `fly.toml` file somewhere before running `wasp build`, and copy it into .wasp/build/ after. When the `fly.toml` file exists in .wasp/build/ dir, you do not need to specify the `--config <path>`.
1. Run `flyctl config save -a <app-name>` to regenerate the `fly.toml` file from the remote state stored in Fly.io.

### Deploying to Heroku (non-free)

:::note
Heroku used to offer free apps under certain limits. However, as of November 28, 2022, they ended support for their free tier. https://blog.heroku.com/next-chapter

As such, we recommend using an alternative provider like [Fly.io](#deploying-to-flyio-free-recommended) for your first apps.
:::

You will need Heroku account, `heroku` CLI and `docker` CLI installed to follow these instructions.

Make sure you are logged in with `heroku` CLI. You can check if you are logged in with `heroku whoami`, and if you are not, you can log in with `heroku login`.

#### Set up a Heroku app (only once per Wasp app)
Unless you already have a heroku app that you want to deploy to, let's create a new Heroku app:
```
heroku create <app-name>
```

Unless you have external Postgres database that you want to use, let's create new database on Heroku and attach it to our app:
```
heroku addons:create --app <app-name> heroku-postgresql:hobby-dev
```
Heroku will also set `DATABASE_URL` env var for us at this point. If you are using external database, you will have to set it yourself.

The `PORT` env var will also be provided by Heroku, so the only two left to set are the `JWT_SECRET` and `WASP_WEB_CLIENT_URL` env vars:
```
heroku config:set --app <app-name> JWT_SECRET=<random_string_at_least_32_characters_long>
heroku config:set --app <app-name> WASP_WEB_CLIENT_URL=<url_of_where_frontend_will_be_deployed>
```

NOTE: If you do not know what your frontend URL is yet, don't worry. You can set `WASP_WEB_CLIENT_URL` after you deploy your frontend.

#### Deploy to a Heroku app
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

:::note
#### Note for Apple M1 users
Apple M1 users need to build a non-Arm image, so the above step will not work at this time. Instead of `heroku container:push`, users instead should:

```bash
docker buildx build --platform linux/amd64 -t <app-name> .
docker tag <app-name> registry.heroku.com/<app-name>/web
docker push registry.heroku.com/<app-name>/web
```

You are now ready to proceed to the next step.
:::

Deploy the pushed image and restart the app:
```
heroku container:release --app <app-name> web
```
This is it, backend is deployed at `https://<app-name>.herokuapp.com`!

Additionally, you can check out the logs with:
```
heroku logs --tail --app <app-name>
```

:::note
#### Note on using pg-boss with Heroku
If you wish to deploy an app leveraging Jobs that use pg-boss as the executor to Heroku, you need to set an additional environment variable called `PG_BOSS_NEW_OPTIONS` to `{"connectionString":"<REGULAR_HEROKU_DATABASE_URL>","ssl":{"rejectUnauthorized":false}}`. This is because pg-boss uses the `pg` extension, which does not seem to connect to Heroku over SSL by default, which Heroku requires. Additionally, Heroku uses a self-signed cert, so we must handle that as well.
- https://devcenter.heroku.com/articles/connecting-heroku-postgres#connecting-in-node-js
:::

## Deploying web client (frontend)
Position yourself in `.wasp/build/web-app` directory (reminder: which you created by running `wasp build` previously):
```
cd .wasp/build/web-app
```
assuming you were at the root of your Wasp project at that moment.

Run
```
npm install && REACT_APP_API_URL=<url_to_wasp_backend> npm run build
```
where <url_to_wasp_backend> is url of the wasp backend that you previously deployed, e.g. `https://wasp-test.fly.dev`.

This will create `build/` directory, which you can deploy to any static hosting provider.
Check instructions below for deploying to Netlify.

### Deploying to Netlify

Netlify is a static hosting solution that is free for many use cases.
You will need Netlify account and `netlify` CLI installed to follow these instructions.

Make sure you are logged in with `netlify` CLI. You can check if you are logged in with `netlify status`, and if you are not, you can log in with `netlify login`.

While positioned in `.wasp/build/web-app/` directory, and after you have created `.wasp/build/web-app/build/` directory as per instructions above, run
```
netlify deploy
```
and carefully follow their instructions (i.e. do you want to create a new app or use existing one, team under which your app will reside, ..., final step to run `netlify deploy --prod`).

That is it!

NOTE: Make sure you set this URL as the `WASP_WEB_CLIENT_URL` environment variable in your server hosting environment (e.g., Fly.io or Heroku).
