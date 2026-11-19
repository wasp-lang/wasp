<!-- This is the user-facing intro of the API reference. For the developer-facing docs, see README.md. -->

# @wasp/spec

The `@wasp/spec` package has everything you need to write your app's [Wasp Spec](/docs/features/spec): the `main.wasp.ts` file where you define your app's pages, routes, queries, actions, auth, and more.

```ts title="main.wasp.ts"
import { app } from "wasp/spec";

export default app({
  name: "todoApp",
  title: "ToDo App",
  spec: [],
});
```

## Getting the package

### In a Wasp app

You don't need to install anything. The package comes with `@wasp/wasp`, which every Wasp app lists in its `package.json`, and you import it as `wasp/spec`.

### In a library

A library imports the package by its own name, `@wasp/spec`. If you are publishing a helper or library for the Wasp Spec, add `@wasp/spec` to both the `devDependencies` and the `peerDependencies` of your `package.json`. That way, you have a local copy during development, and your library uses the version installed in your users' Wasp projects:

```json title="package.json"
{
  "peerDependencies": {
    "@wasp/spec": "{latestWaspVersion}"
  },
  "devDependencies": {
    "@wasp/spec": "{latestWaspVersion}"
  }
}
```
