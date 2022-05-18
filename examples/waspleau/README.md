# Waspleau

Welcome to the Waspleau example! This is a small Wasp project that will allow you to setup an easy Dashboard that pulls in data via Jobs and stores them in the database.

## Step 1

Clone this repo

## Step 2

Update ext/workers with whatever you want to track, add to main.wasp as a `Job`, and optionally import in `serverSetup.js`.

## Step 3 (with PostgreSQL running)

`NODE_ENV=development DATABASE_URL="postgresql://postgres@localhost/waspleau-dev" wasp db migrate-dev`

## Step 4

`NODE_ENV=development DATABASE_URL="postgresql://postgres@localhost/waspleau-dev" wasp start`

This will start your background workers as Wasp Jobs and present a dashboard UI that will auto-refresh every minute.

Note: As you develop your own workers, keep in mind each time you save a file in the project it will automatically reload, hence restarting your server, `Job`s, and server function.
