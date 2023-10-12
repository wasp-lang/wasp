# Waspleau

Welcome to the Waspleau example! This is a small Wasp project that tracks status of wasp-lang/wasp repo via a nice looking dashboard.
It pulls in data via [Jobs](https://wasp-lang.dev/docs/language/features#jobs) and stores them in the database.

This example project can serve as a good starting point for building your own dashboard with Wasp, that regularly pulls in external data by using Jobs Wasp feature.

The deployed version of this example can be found at https://waspleau.netlify.app/ .

## Running in development

1. `wasp start db` to run the development database.
2. `wasp start` to run the app.
3. You might need to do `wasp db migrate-dev` if running the app for the first time or after db changes -> keep an eye out for warning from Wasp.

This will start your background workers as Wasp Jobs and present a dashboard UI that will auto-refresh every minute.

## Modifying the example to track the data of your choice

Update ext/workers with whatever you want to track, add them to main.wasp as a `job`, and optionally import and use them in `serverSetup.js` (or other server-side code).

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
