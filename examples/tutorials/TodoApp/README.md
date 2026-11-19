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

### Installing with the development version of Wasp

In this repo, the example is part of the repo's pnpm workspace and depends on the development version of Wasp, which isn't on npm. Build Wasp from source (see [`waspc/README.md`](../../../waspc/README.md)), which also installs the example's dependencies:

```sh
mise run build
```

Run the commands above with `pnpm` instead of `npm` then. To use the example outside of this repo, set `@wasp/wasp` in its `package.json` to a released Wasp version.
