# TodoApp

The main purpose of this app is for contributors to easily test the version of the `wasp` CLI that they are working on right now and therefore aid them during development.

That is why it is a bit crowded with features and somewhat incoherent: it is not an app to be used as a showcase, but an app that we use as a "workbench" / "playground" while working on `wasp`.

This app should always be "up to date", in the sense that we keep updating it to always work with the latest changes to `wasp`. This means you can count on it being up to date, but also that you should make sure to keep it up to date as you do changes. You should also be adding new features to it if you implement them.

NOTE: This app has no Playwright tests, while on the other hand we have `headless-tests/examples/todoApp` which is similar to this one and has Playwright tests. We plan on merging those two to get the best of both worlds.

## Set up

### Env vars

To do the minimum to get the app running, do:
```sh
cp .env.server.example .env.server
```
This will create a `.env.server` file with some basic env vars set to dummy values. This will allow your app to run and work, but not all features will work, e.g. Google Auth won't work because it needs you to provide real API keys via env vars.

To obtain all the API keys to get all the features of the app working, if you do need that, you will have to obtain them on your own, normally from the corresponding third-party services. Check Wasp docs for instructions on how to do this for Google Auth, GitHub Auth, and so on.

#### Obtaining API keys as a Wasp Team member

If you are a member of the Wasp Team, you don't have to obtain all the additional API keys on your own, but can instead pull the default .env.server file that we share among the team, that has API keys we use exclusively for development.

We are using vault.dotenv.org for storing and sharing this file.

TODO: instructions + set it up.

## Running

Start the database in a separate terminal session:
```
cabal run wasp-cli start db
```

Migrate the database if needed:
```
cabal run wasp-cli db migrate-dev
```

Run the app!
```
cabal run wasp-cli start
```

App will not open in the browser on its own: open localhost:3000 in the browser to check it out.

## Building

TODO: We have tests for building. Say something about those here?
