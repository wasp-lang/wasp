---
title: Deploying
---
import useBaseUrl from '@docusaurus/useBaseUrl';

import AddExternalAuthEnvVarsReminder from './_addExternalAuthEnvVarsReminder.md'

:::info
Wasp is in beta, so keep in mind there might be some kinks / bugs, and possibly a bit bigger changes in the future.
If you encounter any issues, reach out to us on [Discord](https://discord.gg/rzdnErX) and we will make sure to help you out!
:::

# Automated

## Wasp CLI

Using the Wasp CLI, you can easily deploy a new app to [Fly.io](https://fly.io) with just a single command:
```shell
wasp deploy fly launch my-wasp-app mia
```

Under the covers, this runs the equivalent of the following commands:
```shell
wasp deploy fly setup my-wasp-app mia
wasp deploy fly create-db mia
wasp deploy fly deploy
```

In the above commands, we used an app basename of `my-wasp-app` and deployed it to the Miami, Florida (US) region (called `mia`). The basename is used to create all three app tiers, so you will have three components in your Fly dashboard:

- `my-wasp-app-client`
- `my-wasp-app-server`
- `my-wasp-app-db`

:::tip Unique Name
Your app name must be unique across all of Fly or deployment will fail. Additionally, please do not CTRL-C or exit your terminal as these commands run.
:::

:::caution A Note on Free Tiers 💳
Fly has [free allowances](https://fly.io/docs/about/pricing/#plans) for up to 3 VMs (so deploying a Wasp app to a fresh account is free), but all plans require you to add your credit card info before proceeding. If you don't, the deploy will fail!

To do so, go to your [account's billing page](https://fly.io/dashboard/personal/billing).
:::

The list of available Fly regions can be found [here](https://fly.io/docs/reference/regions/). You can also run `wasp deploy fly cmd platform regions --context server`.

### Commands

`setup` will create your client and server apps on Fly, and add some secrets, but does _not_ deploy them. We need a database first, which we create with `create-db`, and it is automatically linked to your server.

:::note
We only run the `setup` and `create-db` steps once.

You may notice after running `setup` you have a `fly-server.toml` and `fly-client.toml` in your Wasp project directory. Those are meant to be version controlled. If you want to maintain multiple apps, you can add the `--fly-toml-dir <abs-path>` option to point to different directories, like "dev" or "staging".
:::

Finally, we `deploy` which will push your client and server live. We run this single command each time you want to update your app.

:::note
Fly.io offers support for both locally built Docker containers and remotely built ones. However, for simplicity and reproducability, the CLI defaults to the use of a remote Fly.io builder. If you wish to build locally, you may supply the `--build-locally` option to `wasp deploy fly launch` or `wasp deploy fly deploy`.
:::

If you would like to run arbitrary Fly commands (eg, `flyctl secrets list` for your server app), you can run them like so:
```shell
wasp deploy fly cmd secrets list --context server
```

:::note
If you are deploying an app that requires any other environment variables (like social auth secrets), you will want to set your environment variables up like so:

During `launch`:
```
wasp deploy fly launch my-wasp-app mia --server-secret GOOGLE_CLIENT_ID=<...> --server-secret GOOGLE_CLIENT_SECRET=<...>
```

After `launch`/`setup`:
```
wasp deploy fly cmd secrets set GOOGLE_CLIENT_ID=<...> GOOGLE_CLIENT_SECRET=<...> --context=server
```
:::

:::note
If you have multiple orgs, you can specify a `--org` option. For example: `wasp deploy fly launch my-wasp-app mia --org hive`
:::

# Manual

In addition to the CLI, you can deploy a Wasp project by generating the code and then deploying generated code "manually", as explained below.

In the future, the plan is to have Wasp take care of it completely: you would declaratively define your deployment in .wasp and then just call `wasp deploy` ([github issue](https://github.com/wasp-lang/wasp/issues/169)).

If you want to deploy your App completely **free** of charge, continue reading below for guides on using Fly.io as your backend (server) provider and Netlify for your frontend (client).

If you prefer to host client and server on **one platform**, and don't mind paying a very small fee for extra features, we suggest following the guide on using [Railway as your provider](#deploying-to-railway-freemium-all-in-one-solution).

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

<AddExternalAuthEnvVarsReminder />

### Deploying to Fly.io (free, recommended)

Fly.io offers a variety of free services that are perfect for deploying your first Wasp app! You will need a Fly.io account and the [`flyctl` CLI](https://fly.io/docs/hands-on/install-flyctl/).

:::note
Fly.io offers support for both locally built Docker containers and remotely built ones. However, for simplicity and reproducability, we will default to the use of a remote Fly.io builder.

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

# If you are using an external auth method (Google or GitHub), make sure to add their vars too!
# flyctl secrets set GOOGLE_CLIENT_ID=<google_client_id>
# flyctl secrets set GOOGLE_CLIENT_SECRET=<google_client_secret>
```

<AddExternalAuthEnvVarsReminder />

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
heroku addons:create --app <app-name> heroku-postgresql:mini
```
:::caution

Heroku does not offer a free plan anymore and `mini` is their cheapest database instance - it costs $5/mo.

:::

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
:::info NO SLASH
Make sure your API URL does <strong>not</strong> have a trailing "/" on the end of it:<br/>
✅ https://backend.example.com <br/>❌ https://backend.example.com/ 
:::

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

## Deploying to Railway ("freemium", all-in-one solution)

Railway is a simple and great way to host your server and database. It's also possible to deploy your entire app -- database, server, and client. You can use the platform for free for a limited time, or if you meet certain eligibility requirements. See their [plans page](https://docs.railway.app/reference/plans) for more info.

:::caution ✋
  Due to Railway's current proxy configuration, client-side routing will not work correctly without some additional configuration, which is described in the toggle below. 
  
  If you prefer NOT to configure your client for Railway, another option is to deploy only your back-end there, while deploying your client to a seperate service such as [Netlify](#deploying-to-netlify)

  <details>
    <summary>
      <em>Additional Configuration for Client-Side Routing</em>
    </summary>
    <div>

1. Ensure your Wasp project is built by running `wasp build` in the project dir.
2. Go to `/.wasp/build/web-app` and create 2 files:

- Dockerfile: simply create a file named `Dockerfile` with the contents below (note, it's important to use `FROM nginx:1.19.10-alpine`, as using anything other than this version may throw a lot of exceptions on the server):
```dockerfile
FROM node:18-alpine AS builder

# Examples of any ENV variables that build requires for react app to have
ARG PORT
ARG WASP_WEB_CLIENT_URL
ARG REACT_APP_API_URL
ARG API_URL

ENV REACT_APP_PORT=$PORT
ENV REACT_APP_WASP_WEB_CLIENT_URL=$WASP_WEB_CLIENT_URL
ENV REACT_APP_API_URL=$REACT_APP_API_URL

# Add a work directory
WORKDIR /app

COPY package.json .

RUN npm install

COPY . /app/

RUN npm run build

FROM nginx:1.19.10-alpine

# Set working directory to nginx asset directory
WORKDIR /usr/share/nginx/html

# Remove default nginx static assets
RUN rm -rf ./*

COPY --from=builder /app/build .

COPY .nginx/nginx.conf /etc/nginx/conf.d/default.conf

EXPOSE 3000

ENTRYPOINT ["nginx", "-g", "daemon off;"]
```
- Create a file calle `.dockerignore` with the following contents:
```
node_modules
```
3. Create a new directory `/.nginx` inside the `web-app` dir.
4. Create a file `nginx.conf` inside the `/.nginx` dir with the following contents:
```
server {
   listen       8080;
   server_name  localhost;

   location / {
       root   /usr/share/nginx/html;
       index  index.html;
       try_files $uri $uri/ /index.html;
   }
}
```
5. Make sure that the `client` service in Railway, which you will set up below, has env variable `PORT` set to `8080` (see the [Add Enviornment Variables](/docs/deploying#add-environment-variables) section below for more info).
6. Optionally, you may need to disable `tsc` in the `/.wasp/build/web-app/package.json` file if Docker fails on deploy due to typing issues:
```
	"scripts": {
		"start": "npm run validate-env && vite",
		"build": "npm run validate-env && vite build",
		"validate-env": "node -r dotenv/config ./scripts/validate-env.mjs"
	},
```
7. Then continue to follow the instructions below to deploy your app to Railway.

</div>
  </details>
:::

To get started, follow these steps:

1. [Generate deployable code](#generating-deployable-code) (`wasp build`)
2. Sign up at [Railway.app](https://railway.app) (Tip! Sign up with your GitHub account to be elligble for the free tier)
3. Before creating a new project, install the [Railway CLI](#https://docs.railway.app/develop/cli#install) by running the following command in your terminal:
  ```shell
  curl -fsSL https://railway.app/install.sh | sh
  ```
4. While still in the terminal, run `railway login` and a browser tab will open to authenticate you.

#### Create New Project

Go back to your [Railway dashboard](https://railway.app/dashboard), click on **+ New Project**, and select `Provision PostgreSQL` from the dropdown menu.

Once it initializes, right click on the `+ New` button in the top right corner and select `>_ Empty Service`. Once it initializes, click on it, go to `Settings > General` and change the name (e.g. `server`).

Go ahead and create another empty service and name it (e.g. `client`).

<details>
  <summary>
    <em>Just in case, here is a helpful screenshot ;)</em>
  </summary>
  <div>
    <img alt="Create an Empty Service"
      src={useBaseUrl('img/deploying/railway-rename.png')} />
 </div>
</details>

#### Deploy to services

Now go back to your terminal and execute the following commands:

  1. Move into your app's `.wasp/build/` directory, which was created when you ran `wasp build` previously:
  ```shell
  cd .wasp/build
  ```
  2. "Link" your app build to your newly created Railway project:
  ```shell
  railway link
  ```
  3. Push and deploy the project to railway (make sure you're in `.wasp/build`):
  ```shell
  railway up
  ```
  Select `server` when prompted with `Select Service`. Press enter.
  Railway will now locate the Dockerfile and deploy your server 👍

  When deployment is finished, you might see: `Deployment live at <url_to_wasp_backend>`. If not, go now to your [Railway dashboard](https://railway.app/dashboard) and in the server instance's `Settings` tab, click `Generate Domain`. Copy the new URL as we will need it for step 5! 📜

  4. Next, change into your app's frontend build directory `.wasp/build/web-app`:
  ```shell
  cd web-app
  ```
  5. Create the production build, adding the URL from step 3:
  ```shell
  npm install && REACT_APP_API_URL=<url_to_wasp_backend> npm run build
  ```
  :::info NO SLASH
  Make sure your API URL does <strong>not</strong> have a trailing "/" on the end of it:<br/>
  ✅ https://backend.example.com <br/>❌ https://backend.example.com/ 
  :::

  6. Change into the `.wasp/build/web-app/build` directory:
  ```shell
  cd build 
  ```
  7. Next, we want to link this specific frontend directory to our project as well:
  ```shell
  railway link 
  ```
  8. Deploy the client and select `client` when prompted with `Select Service`:
  ```shell
  railway up
  ```
  9. Your apps are deployed 🧙‍♂️. Now it's time to add environment variables, so open the project in the browser
  ```shell
  railway open
  ```

#### Add Environment Variables

Back in your [Railway dashboard](https://railway.app/dashboard), click on your project and you should see your newly deployed services: Postgres, Server, and Client.

Now you're going to pass each service the correct [environment variables](#env-vars). To do this, you first need to tell Railway to generate public domains for client and server. 

Go to the server instance's `Settings` tab, and click `Generate Domain`. Do the same under the client's `Settings`.

The Postgres database is already initialized with a domain, so click on the Postgres instance, go to the **Connect** tab and copy the `Postgres Connection URL`. 

Go back to your `server` instance and navigate to its `Variables` tab. Now add the copied Postgres URL as `DATABASE_URL`, as well as the client's domain as `WASP_WEB_CLIENT_URL`.

<AddExternalAuthEnvVarsReminder />
 
Next, copy the server's domain, move over to the client's `Variables` tab and add the generated server domain as a new variable called `REACT_APP_API_URL`. 

<details>
  <summary>
    <em>Having trouble finding these settings?</em>
  </summary>

  <div>
    <figure>
      <img src={useBaseUrl('img/deploying/railway-postgres-url.png')}/>
      <figcaption class="image-caption">Postgres Connection URL</figcaption>
    </figure>
    <figure>
      <img src={useBaseUrl('img/deploying/railway-server-var.png')}/>
      <figcaption class="image-caption">Env Variables</figcaption>
    </figure>
 </div>
</details>

And now you should be deployed! 🐝 🚂 🚀

#### Updates & Redeploying
When you make updates and need to redeploy, just follow [steps 3-7](#deploy-to-services) above. Remember, you can connect or disconnect your app to any project in your Railway account by using `railway link` or `railway unlink` from within the app's directory.

## Customizing the Dockerfile
By default, Wasp will generate a multi-stage Dockerfile that is capable of building an image with your Wasp-generated server code and running it, along with any pending migrations, as in the deployment scenario above. If you need to customize this Dockerfile, you may do so by adding a Dockerfile to your project root directory. If present, Wasp will append the contents of this file to the _bottom_ of our default Dockerfile.

Since the last definition in a Dockerfile wins, you can override or continue from any existing build stages. You could also choose not to use any of our build stages and have your own custom Dockerfile used as-is. A few notes are in order:
- if you override an intermediate build stage, no later build stages will be used unless you reproduce them below
- the contents of the Dockerfile are dynamic, based on the features you use, and may change in future releases as well, so please verify the contents have not changed from time to time
- be sure to supply an `ENTRYPOINT` in your final build stage or it will not have any effect

To see what your project's (potentially combined) Dockerfile will look like, run: `wasp dockerfile`

Here are the official docker docs on [multi-stage builds](https://docs.docker.com/build/building/multi-stage/). Please join our Discord if you have any questions, or if the customization hook provided here is not sufficient for your needs!
