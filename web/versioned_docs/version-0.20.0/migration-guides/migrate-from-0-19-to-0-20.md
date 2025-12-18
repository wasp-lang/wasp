---
title: Migration from 0.19.X to 0.20.X
---

To install the latest version of Wasp on Linux / OSX / WSL (Windows), open your terminal and run:
```sh
curl -sSL https://get.wasp.sh/installer.sh | sh
```
If you're reading this far into the future when Wasp 0.20.0 is no longer the newest version of Wasp, you can pass a version argument to the install script:
```sh
curl -sSL https://get.wasp.sh/installer.sh | sh -s -- -v 0.20.0
```

## What's new in 0.20.X?

### Wasp now uses React 19

Wasp now uses the latest version of React, bringing many new improvements and features. From the React team:
> The improvements added to React 19 require some breaking changes, but we’ve worked to make the upgrade as smooth as possible, and we don’t expect the changes to impact most apps.

## How to migrate?

To migrate your Wasp app from 0.19.X to 0.20.X, follow these steps:

### 1. Bump the Wasp version

Update the version field in your Wasp file to `^0.20.0`:

```wasp title="main.wasp"
app MyApp {
  wasp: {
    // highlight-next-line
    version: "^0.20.0"
  },
}
```

### 2. Update `package.json` dependencies to versions compatible with React 19

Bump the version numbers for the React dependencies in your `package.json` file:

```json title="package.json"
{
  // ...
  "dependencies": {
    // ...
    // highlight-next-line
    "react": "^19.2.1",
    // highlight-next-line
    "react-dom": "^19.2.1",
  },
  "devDependencies": {
    // ...
    // highlight-next-line
    "@types/react": "^19.2.7",
    // highlight-next-line
    "@types/react-dom": "^19.2.3",
  }
}
```

To complete the dependency updates, run the following commands in your terminal:

```bash
wasp clean
rm package-lock.json
wasp ts-setup # ONLY if you are using the Wasp TS Config
```

### 3. Update your code to work with React 19
The easiest way to update your code to work with React 19 is following their official guide: https://react.dev/blog/2024/04/25/react-19-upgrade-guide. There aren't many breaking changes so the update should be pretty smooth.

### 4. Update your app to work with `@testing-library/react` 16.x.x
Search your codebase for `@testing-library/react` and fix any potential errors around its usage. Check their changelogs for breaking changes introduced since the last version (14.x.x):
  - https://github.com/testing-library/react-testing-library/releases/tag/v15.0.0
  - https://github.com/testing-library/react-testing-library/releases/tag/v16.0.0

If the search returns no results, it means you aren't using this feature and there's nothing to update.

### 5. Enjoy your updated Wasp app

That's it!
