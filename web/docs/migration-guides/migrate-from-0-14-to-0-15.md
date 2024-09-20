---
title: Migration from 0.14.X to 0.15.X
---

## What's new in 0.15.0?

### TS SDK for Wasp (preview)

TODO: write this section

### Prisma 5

TODO: write this section

### React Router 6

TODO: write this section

## How to migrate?

To migrate your app to Wasp 0.15.x, you must:

1.  Bump the version in `main.wasp` ...

### Bump the version 

Let's start with something simple. Update the version field in your Wasp file to `^0.15.0`:

```wasp title="main.wasp"
app MyApp {
  wasp: {
    // highlight-next-line
    version: "^0.15.0"
  },
}
```

That's it!

You should now be able to run your app with the new Wasp 0.15.0.