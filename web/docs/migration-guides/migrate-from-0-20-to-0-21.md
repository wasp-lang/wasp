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

### 2. Upgrade Tailwind CSS to v4

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

1. Add the `@tailwindcss/vite` plugin to your `vite.config.js` file.

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

### 3. Enjoy your updated Wasp app

That's it!
