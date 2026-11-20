<!-- This is the user-facing intro of the API reference. For the developer-facing docs, see README.md. -->

# @wasp/wasp

The `@wasp/wasp` package is what your project depends on to use Wasp. It brings the Wasp CLI and the packages your app needs. Its modules below are the ones you import in your config files:

```ts title="vite.config.ts"
import { wasp } from "@wasp/wasp/vite";
```

The code you import from your app's source files is in the [`wasp`](/docs/api/wasp) package, and the Wasp Spec is in [`@wasp/spec`](/docs/api/@wasp/spec).

## Getting the package

Every Wasp app lists `@wasp/wasp` in the `dependencies` of its `package.json`, so you don't need to install anything else.
