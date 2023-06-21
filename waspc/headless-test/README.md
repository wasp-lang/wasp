## Headless Test

We are testing if a simple scenario works for the user each time:
- signup
- login
- create a new task

Check out the test file: `tests/simple.spec.ts`

### How to run

Running headless tests:
```
$ cd headless-test
$ npm install
$ npx playwright test
```

If something breaks, maybe the example app won't run. Try running it and see if there are any errors:
```
npm run example-app:start
```

### How to run in CI

We set up a GitHub Action to run the test in CI. See `.github/workflows/ci.yml` for details.
