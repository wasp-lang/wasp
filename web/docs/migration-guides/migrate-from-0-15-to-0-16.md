---
title: Migration from 0.15.X to 0.16.X
---

## What's new in 0.16.0?

TODO: write this part

## How to migrate?

To migrate your Wasp app from 0.15.X to 0.16.X, follow these steps:

### 1. Bump the Wasp version

Update the version field in your Wasp file to `^0.16.0`:

```wasp title="main.wasp"
app MyApp {
  wasp: {
    // highlight-next-line
    version: "^0.16.0"
  },
}
```

### 2. Update the `package.json` file

TODO: write this part

That's it!

You should now be able to run your app with the new Wasp 0.16.0.
