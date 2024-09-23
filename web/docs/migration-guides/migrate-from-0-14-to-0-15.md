---
title: Migration from 0.14.X to 0.15.X
---

## What's new in 0.15.0?

Wasp 0.15.0 brings some exciting experimental features and upgrades to the existing ones. Let's see what's new.

### TS SDK for Wasp (preview)

TODO: @sodic has to write this part

### Prisma 5

Wasp is now using the latest Prisma 5 version, which brings a lot of performance improvements and new features.

From the Prisma docs:
> Prisma ORM 5.0.0 introduces a number of changes, including the usage of our new JSON Protocol, which make Prisma Client faster by default.

This means that your Wasp app will be faster and more reliable with the new Prisma 5 version.

### React Router 6

Wasp also upgraded its React Router version from `5.3.4` to `6.26.2`. This means that we are now using the latest React Router version, which brings up to speed and opens up new possibilities for Wasp e.g. potentially using loaders and actions in the future.

There are some breaking changes in React Router 6, so you will need to update your app to use the new hooks and components.

## How to migrate?

To migrate your app to Wasp 0.15.x, you must:

1. Bump the version in `main.wasp` ...
2. Update the `prisma` version in `package.json` to `5.19.1`
3. Migrate from old React Router 5 APIs to new React Router 6 APIs

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

### Update your `package.json` file

Update the `prisma` version in your `package.json` file to `5.19.1`:

```json title="package.json"
{
  "dependencies": {
    "prisma": "5.19.1"
  }
}
```

### Use latest React Router APIs

- `useHistory()` -> `useNavigate()`
- `<Redirect />` -> `<Navigate />`
- `props.match.params` -> `useParams()`

### Update your root component

- Wasp's root component feature requires rendering `<Outlet />` instead of rendering `children`

```tsx title="src/App.tsx"
import { Outlet } from 'react-router-dom';

...
```

That's it!

You should now be able to run your app with the new Wasp 0.15.0.