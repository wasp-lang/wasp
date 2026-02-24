---
title: Migration from 0.18.X to 0.19.X
---

## What's new in 0.19.0?

### Wasp now uses npm workspaces

Wasp now enables npm workspaces for managing the generated app. This change makes our dependency system more reliable, and better prepares us for future features. It also makes installs faster overall and reduces the size of each project on your disk. This is largely transparent, and you shouldn't notice any difference in how you develop your app.

### The type of `config.allowedCORSOrigins` has changed

The type of `config.allowedCORSOrigins` (imported from `wasp/server`) was changed from `string | string[]` to `(string | RegExp)[]`, which is always an array. Now, it's simpler to extend our default CORS rules if you just want to add extra domains to the list.

## How to migrate?

To migrate your Wasp app from 0.18.X to 0.19.X, follow these steps:

### 1. Bump the Wasp version

Update the version field in your Wasp file to `^0.19.0`:

```wasp title="main.wasp"
app MyApp {
  wasp: {
    // highlight-next-line
    version: "^0.19.0"
  },
}
```

### 2. Add the `workspaces` key to your `package.json`

Add the following key to your `package.json` file:

```json title="package.json"
{
  // highlight-next-line
  "workspaces": [".wasp/build/*", ".wasp/out/*"]
}
```

And, to recalculate the dependencies with the new workspace setup, run the following commands in your terminal:

```bash
wasp clean
rm package-lock.json
wasp ts-setup # ONLY if you are using the Wasp TS Config
```

### 3. Fix type errors caused by `config.allowedCORSOrigins`
Search your codebase for the string `allowedCORSOrigins` and fix any potential type errors around its usage. You can follow [our middleware guide](../advanced/middleware-config.md) for the recommended way to extend the CORS configuration.

If the search returns no results, it means you aren't using this feature and there's nothing to fix.

### 4. Enjoy your updated Wasp app

That's it!
