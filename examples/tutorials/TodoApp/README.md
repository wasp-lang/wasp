# Todo App Tutorial Example App

This is an example app that complements the [Intro Wasp Tutorial](https://wasp.sh/docs/tutorial/create). It is a simple Todo App that demonstrates how to use Wasp to build a truly full-stack web app quickly and easily.

This example app represents the final state of the tutorial. If you'd prefer to follow it step-by-step, you can find [the tutorial here](https://wasp.sh/docs/tutorial/create).

## Running the App Locally

After you've cloned this repository, you can run the app locally by following these steps:

Position yourself in the project directory, install the dependencies, which include Wasp, and migrate the DB:

```bash
npm install
npm run wasp -- db migrate-dev
```

Finally, run the app:

```bash
npm run dev
```

### Installing on the `main` branch

<!-- TODO(#2262): Remove this section once the examples join the pnpm workspace. -->

On `main`, this example depends on the unreleased Wasp in this repo, so `npm install` fails. Build Wasp from source instead (see [`waspc/README.md`](../../../waspc/README.md)), and then install against it from this directory:

```sh
mise run build
mise run reinstall-dev-wasp-app-deps
```
