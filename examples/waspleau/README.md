# Waspleau

Welcome to the Waspleau example! This is a small Wasp project that will allow you to setup an easy Dashboard that pulls and stores data in-memory using scheduled jobs on a Redis-backed Bull queue.

Note: The commands below assume we are using PostgreSQL, but this is only due to a known bug that requires a model to use queries (even though we don't use it here). We are tracking it here: https://github.com/wasp-lang/wasp/issues/398

## Step 1

Clone this repo

## Step 2

Update ext/workers with whatever you want to track and import in `serverSetup.js`

## Step 3 (with PostgreSQL running)

`NODE_ENV=development DATABASE_URL="postgresql://postgres@localhost/waspleau-dev" wasp db migrate-dev`

## Step 4

`NODE_ENV=development DATABASE_URL="postgresql://postgres@localhost/waspleau-dev" wasp start`

This will start your background workers and present a dashboard UI that will auto-refresh every minute.

Note: As you develop your own workers, keep in mind each time you save a file in the project it will automatically reload, hence restarting your server and server function. Use the `MOCK_DATA=true` command line envar if you want to avoid pinging external resources.

Warning: In development mode, your queue is cleared each time you start your server. If you change a repeating Job's name or cron schedule in production, please make sure to obliterate the queue via `OBLITERATE_QUEUE=true` ENVAR. Otherwise, your old Jobs will still be active and run more frequently than you expect! You can alternatively leverage https://github.com/OptimalBits/bull/blob/master/REFERENCE.md#queueremoverepeatable but that would require updating `serverSetup.js` accordingly.
