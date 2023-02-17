# Waspleau

Welcome to the Waspleau example! This is a small Wasp project that will allow you to setup an easy Dashboard that pulls in data via [Jobs](https://wasp-lang.dev/docs/language/features#jobs) and stores them in the database.

The deployed version of this example can be found at https://waspleau.netlify.app/

## Step 1

Clone this repo

## Step 2

Update ext/workers with whatever you want to track, add them to main.wasp as a `job`, and optionally import and use them in `serverSetup.js` (or other server-side code).

## Step 3 (with PostgreSQL running)

`NODE_ENV=development DATABASE_URL="postgresql://postgres@localhost/waspleau-dev" wasp db migrate-dev`

## Step 4

`NODE_ENV=development DATABASE_URL="postgresql://postgres@localhost/waspleau-dev" wasp start`

This will start your background workers as Wasp Jobs and present a dashboard UI that will auto-refresh every minute.

Note: As you develop your own workers, keep in mind each time you save a file in the project it will automatically reload everything, including restarting your server, which may re-submit or terminate running `job`s.

## Heroku Deployment Note

If you wish to deploy this on Heroku, pg-boss will fail to initialize as for some reason the `pg` client does not attempt to connect over SSL by default. This results in an error like:

```
pg-boss failed to start!
2022-05-18T19:07:37.464126+00:00 app[web.1]: error: no pg_hba.conf entry for host "***", user "***", database "***", SSL off
```

Ref: https://help.heroku.com/DR0TTWWD/seeing-fatal-no-pg_hba-conf-entry-errors-in-postgres

Even if you force SSL, it will still fail as Heroku uses a self-signed certificate. The solution is to set a `PG_BOSS_NEW_OPTIONS` environment variable to something like this:

```
{"connectionString":"<your-heroku-DATABASE_URL>","ssl":{"rejectUnauthorized":false}}
```

Ref: https://devcenter.heroku.com/articles/connecting-heroku-postgres#connecting-in-node-js
