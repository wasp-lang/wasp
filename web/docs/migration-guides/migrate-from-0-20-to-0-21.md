---
title: Migration from 0.20.X to 0.21.X
---

To install the latest version of Wasp, open your terminal and run:

```sh
curl -sSL https://get.wasp.sh/installer.sh | sh
```

If you want to install Wasp 0.21.0 specifically, you can pass a version argument to the install script:

```sh
curl -sSL https://get.wasp.sh/installer.sh | sh -s -- -v 0.21.0
```

## What's new in 0.21.X?

### Upgrade to Vitest 4

Wasp has upgraded its testing framework from Vitest 1 all the way to Vitest 4. This brings a lot of improvements, especially in terms of performance and stability. Most users should not notice any breaking changes, but if you have custom test setups or configurations, please refer to the [Vitest migration guide](https://vitest.dev/guide/migration.html) for more details.

## How to migrate?

To migrate your Wasp app from 0.20.X to 0.21.X, follow these steps:

### 1. Bump the Wasp version

Update the version field in your Wasp file to `^0.21.0`:
```wasp title="main.wasp"
app MyApp {
  wasp: {
    // highlight-next-line
    version: "^0.21.0"
  },
}
```

### 2. Upgrade Vitest tests to v4

**If you don't have test files in your project, you can skip this step.**

We upgraded our testing support from Vitest v1 to Vitest v4. We expect most tests to work without any changes, but if you have custom test setups or configurations, please upgrade your tests by following the migration guides from Vitest, in order:

:::note
Most of the breaking changes are related to internal configuration, edge cases, and very advanced usage, so we recommend first to try running your tests after bumping the Wasp version, and only read through the migration guides if you encounter issues.
:::

1. Vitest v1 to v2: https://v3.vitest.dev/guide/migration.html#vitest-2
2. Vitest v2 to v3: https://v3.vitest.dev/guide/migration.html#vitest-3
3. Vitest v3 to v4: https://vitest.dev/guide/migration.html#vitest-4

### 3. Enjoy your updated Wasp app

That's it!
