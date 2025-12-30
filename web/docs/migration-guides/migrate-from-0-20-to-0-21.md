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

### Better Tailwind CSS support

With this change, we will not require you to upgrade Tailwind CSS in lockstep with Wasp anymore. You can use any version of Tailwind CSS v4 or newer in your Wasp app, and upgrade it (or not) at your own pace.

In previous versions of Wasp, we used a custom way of handling Tailwind CSS configuration files, which tightly coupled us to a specific version. Due to the new Vite installation method in version 4, we can simplify our support, and remove all custom steps. Now Tailwind CSS is just a regular dependency in your Wasp app like any other.

### Merged the `.wasp/out` and `.wasp/build` directories

In previous versions of Wasp, there were two separate directories for generated code: `.wasp/out` (used in development mode) and `.wasp/build` (used in production mode). Starting from Wasp 0.21.X, only the `.wasp/out` directory is used for generated code in both development and production modes. This change simplifies the project structure and reduces confusion.

### Upgraded to Vitest 4

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

### 2. Remove the `.wasp/build/*` workspace from your project

We don't use the `.wasp/build/*` directories anymore, so you should remove them from the list of workspaces in your `package.json` file. Keep the `.wasp/out/*` workspace as it is still needed:

<Tabs>
<TabItem value="before" label="Before">

```json title="package.json"
{
  "workspaces": [
    // highlight-next-line
    ".wasp/build/*",
    ".wasp/out/*"
  ]
}
```

</TabItem>
<TabItem value="after" label="After">

```json title="package.json"
{
  "workspaces": [
    ".wasp/out/*"
  ]
}
```

</TabItem>
</Tabs>

Now, we will clean up the old `.wasp/build` directory (to avoid any potential conflicts), and let `npm` pick up the new workspace configuration. You can do this by running the following command in your terminal:

```sh
wasp clean
wasp ts-setup # if you're using the TS Spec
wasp compile
```

### 23 Upgrade Tailwind CSS to v4

**If you don't have a `tailwindcss` dependency in your `package.json`, you can skip this step.**

1. Run the Tailwind CSS upgrade tool:

    ```sh
    npx @tailwindcss/upgrade
    ```

    It will update any changed classes to the new name, and migrate your config file to the new CSS format.

1. Remove the `@tailwindcss/postcss` plugin from your `postcss.config.js` file.

    <Tabs>
    <TabItem value="before" label="Before">

    ```js title="postcss.config.js"
    export default {
      plugins: {
        // highlight-next-line
        '@tailwindcss/postcss': {},
      },
    };
    ```

    </TabItem>
    <TabItem value="after" label="After">

    ```js title="postcss.config.js"
    export default {
      plugins: {},
    };
    ```

    If the file is now empty, you can delete it entirely.

    </TabItem>
    </Tabs>

1. Uninstall the `@tailwindcss/postcss` package and install `@tailwindcss/vite`.

    ```sh
    npm un @tailwindcss/postcss
    npm i -D @tailwindcss/vite
    ```

1. Add the `@tailwindcss/vite` plugin to your `vite.config.{js,ts}` file.

    ```ts title="vite.config.ts"
    // highlight-next-line
    import tailwindcss from '@tailwindcss/vite';
    import { defineConfig } from 'vite';

    export default defineConfig({
      plugins: [
        // highlight-next-line
        tailwindcss(),
      ],
    });
    ```

1. Find the CSS file with the `@import "tailwindcss"` directive and add `source(".")` to it.

    If you used the default starter templates, the file will be `src/Main.css` or `src/App.css`. If it is in a different location, adjust the path in `source()` accordingly, so that it points to your `src` folder.

    <Tabs>
    <TabItem value="before" label="Before">

    ```css title="src/Main.css" {1}
    @import "tailwindcss";

    /* ... */
    ```

    </TabItem>
    <TabItem value="after" label="After">

    ```css title="src/Main.css" {1}
    @import "tailwindcss" source(".");

    /* ... */
    ```

    </TabItem>
    </Tabs>

If you hit any snags or would like more details, check out the official [Tailwind CSS v4 upgrade guide](https://tailwindcss.com/docs/upgrade-guide), and our updated [Tailwind documentation](../project/css-frameworks.md#tailwind).

### 4. Update your custom Dockerfile

**If you don't have a `Dockerfile` in your project folder, you can skip this step.**

If you have a custom `Dockerfile` in your project, you need to update it to reference the new `.wasp/out` directory instead of the removed `.wasp/build` directory.

This can be a quite straightforward find-and-replace operation:
- **Find** `.wasp/build`
- **Replace** with `.wasp/out`

### 5. Update your custom deployment scripts

**If you use `wasp deploy fly` or `wasp deploy railway` to deploy your app, you can skip this step.**

If you have custom deployment scripts, you'll need to update them to reference the new `.wasp/out` directory instead of the removed `.wasp/build` directory.

This can be a quite straightforward find-and-replace operation:
- **Find** `.wasp/build`
- **Replace** with `.wasp/out`

You can check our updated [deployment methods guide](../deployment/deployment-methods/overview.md) and [CI/CD guide](../deployment/ci-cd.md) for reference on the correct deployment steps.

### 6. Upgrade Vitest tests to v4

**If you don't have test files in your project, you can skip this step.**

We upgraded our testing support from Vitest v1 to Vitest v4. Most of the breaking changes are related to internal configuration, edge cases, or very advanced usage; so we recommend **first to try running your tests after bumping the Wasp version**, and only read through the migration guides if you encounter issues:

1. [Migration guide from Vitest v1 to v2](https://v3.vitest.dev/guide/migration.html#vitest-2)
2. [Migration guide from Vitest v2 to v3](https://v3.vitest.dev/guide/migration.html#vitest-3)
3. [Migration guide from Vitest v3 to v4](https://vitest.dev/guide/migration.html#vitest-4)

### 7. Enjoy your updated Wasp app

That's it!
