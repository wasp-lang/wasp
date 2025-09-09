# TodoApp

The main purpose of this app is for contributors to easily test the version of the `wasp` CLI that they are working on right now and therefore aid them during development.

That is why it is a bit crowded with features and somewhat incoherent: it is not an app to be used as a showcase, but an app that we use as a "workbench" / "playground" while working on `wasp`.

This app should always be "up to date", in the sense that we keep updating it to always work with the latest changes to `wasp`. This means you can count on it being up to date, but also that you should make sure to keep it up to date as you do changes. You should also be adding new features to it if you implement them, so they can easily be tested / tried out.

## Set up

### Env vars

To do the minimum to get the app running, do:

```sh
cp .env.server.example .env.server
```

This will create a `.env.server` file with some basic env vars set to dummy values. This will allow your app to run and work, but not all features will work, e.g. Google Auth won't work because it needs you to provide real API keys via env vars.

To obtain all the API keys to get all the features of the app working, if you do need that, you will have to obtain them on your own, normally from the corresponding third-party services. Check Wasp docs for instructions on how to do this for Google Auth, GitHub Auth, and so on.

#### Obtaining API keys as a Wasp Team member

If you are a member of the Wasp Team, you don't have to obtain all the API keys on your own.

Instead, you can run `npm run env:pull`: this will obtain `.env.server` file that we share on the team level.  
We are using https://vault.dotenv.org to power this and have an account/organization up there.  
If you modify `.env.server` and want to persist the changes (for yourself and for the other team members), do `npm run env:push`.

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

Open `localhost:3000` in the browser to see the app!  
NOTE: Normally wasp apps open automatically in browser, to help new users find their way, but that can become annoying after some time so here we turned that off.

## Testing

### Compiling and building

Run `./ensure_app_compiles_and_builds.sh` to verify the app correctly compiles (TS) and builds (both client and server).

This is also run in CI.

### E2E Tests

#### How to run

Running e2e tests:

```
$ cd e2e-tests
$ npm install
$ npx playwright install --with-deps
$ DEBUG=pw:webserver npx playwright test
```

For debugging it's useful to pass the `ui` flag:

```
$ DEBUG=pw:webserver npx playwright test --ui
```

If something breaks, maybe the example app won't run. Try running it and see if there are any errors:

```
npm run test
```

#### How to run in CI

We set up a GitHub Action to run the tests in CI. See `.github/workflows/waspc-ci.yaml` for details.
