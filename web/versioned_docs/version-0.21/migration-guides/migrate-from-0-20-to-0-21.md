---
title: Migration from 0.20.X to 0.21.X
---

import NetlifyTomlConfig from '../deployment/deployment-methods/\_netlify-toml-config.md'
import InstallInstructions from './_install-instructions.md'
import LegacyInstallerMigration from './_legacy_installer_migration.md'

<LegacyInstallerMigration />

<InstallInstructions version="0.21" />

## What's new in 0.21.X?

### New npm-based installation

Starting from Wasp 0.21, we've switched Wasp to be installed through npm. This simplifies the installation process, makes it easier to manage Wasp versions, and allows npm-centric workflows like mirroring and vetting packages.

We strongly discourage using both installation methods at the same time, as it can lead to potential conflicts over which version is used when calling the `wasp` CLI. The legacy and npm installers will try to detect each other and refuse to run if the other method is already in use.

To switch from the old legacy installer to the npm method, you can run our migration tool:

```sh
curl -sSL https://get.wasp.sh/installer.sh | sh -s -- migrate-to-npm
```

The tool will uninstall the old version of Wasp, and guide you through installing the new version through npm.

For your convenience, we have also published Wasp 0.20.2 to npm so you can keep developing on projects that haven't been upgraded yet. Just tell npm which version you need:

```sh
npm i -g @wasp.sh/wasp-cli@0.20
npm i -g @wasp.sh/wasp-cli@0.21
```

:::caution Versions older than 0.20.2 are not supported
Wasp versions older than 0.20.2 are not available through the npm installer. If you have projects using an older version, you should keep using the installer and upgrade to a npm-supported version as soon as possible.
:::

If you want to learn more about this migration or troubleshoot any problems you might find, read our [Legacy Installer guide](../guides/legacy/installer.md).

### User-land Vite configuration

Wasp has significantly overhauled how the client app is built. Your project directory is now the client app directory, and Vite runs directly from it instead of from `.wasp/out/web-app`.

You now have **full control** over your `vite.config.ts` file. Wasp no longer manages this file internally. Instead, you must import and use the `wasp()` plugin from `wasp/client/vite`, which provides all essential Wasp features:

- Configuration required for Wasp full-stack apps.
- Environment variables validation.
- Prevention of server imports in client code.
- TypeScript type checking during production builds.

### Better Tailwind CSS support

With this change, we will not require you to upgrade Tailwind CSS in lockstep with Wasp anymore. You can use any version of Tailwind CSS v4 or newer in your Wasp app, and upgrade it (or not) at your own pace.

In previous versions of Wasp, we used a custom way of handling Tailwind CSS configuration files, which tightly coupled us to a specific version. Due to the new Vite installation method in version 4, we can simplify our support, and remove all custom steps. Now Tailwind CSS is just a regular dependency in your Wasp app like any other.

### Merged the `.wasp/out` and `.wasp/build` directories

In previous versions of Wasp, there were two separate directories for generated code: `.wasp/out` (used in development mode) and `.wasp/build` (used in production mode). Starting from Wasp 0.21.X, only the `.wasp/out` directory is used for generated code in both development and production modes. This change simplifies the project structure and reduces confusion.

### Upgraded to React Router 7

Wasp has upgraded from React Router 6 to React Router 7. The only change you should notice is that the package has been renamed from `react-router-dom` to `react-router`, so you'll need to update your `package.json` and imports accordingly.

### Upgraded to Vitest 4

Wasp has upgraded its testing framework from Vitest 1 all the way to Vitest 4. This brings a lot of improvements, especially in terms of performance and stability. Most users should not notice any breaking changes, but if you have custom test setups or configurations, please refer to the [Vitest migration guide](https://vitest.dev/guide/migration.html) for more details.

### New `--custom-server-url` option for deployment

The `REACT_APP_API_URL` environment variable is no longer supported for specifying a custom server URL during deployment. Instead, use the new `--custom-server-url` CLI option:

```sh
wasp deploy fly deploy --custom-server-url https://my-custom-server.com
wasp deploy railway deploy myproject --custom-server-url https://my-custom-server.com
```

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

### 2. Add the `wasp()` plugin to your `vite.config.ts`

The `wasp()` plugin is **required** and must be the **first plugin** in your Vite configuration:

```ts title="vite.config.ts"
// highlight-next-line
import { wasp } from 'wasp/client/vite'
import tailwindcss from '@tailwindcss/vite'
import { defineConfig } from 'vite'

export default defineConfig({
  plugins: [
    // highlight-next-line
    wasp(),
    tailwindcss()
  ],
  server: {
    open: true,
  },
})
```

### 3. Update your `package.json`

We've rearranged our workspace architecture a bit, so you'll need to update the `dependencies` and `workspaces` fields in your `package.json` file.

- In your `workspaces` array:
  - Remove `.wasp/build/*`.
  - Add `.wasp/out/sdk/wasp`.
- In your `dependencies` object:
  - Remove the `wasp` dependency.
  - Rename `react-router-dom` to `react-router` (and update the version).

<Tabs>
<TabItem value="before" label="Before">

```json title="package.json"
{
  "workspaces": [
    // highlight-next-line
    ".wasp/build/*",
    ".wasp/out/*"
  ],
  "dependencies": {
    // highlight-next-line
    "react-router-dom": "^6.26.2",
    // ... other dependencies ...
    // highlight-next-line
    "wasp": "file:.wasp/out/sdk/wasp"
  }
  // ... other fields ...
}
```

</TabItem>
<TabItem value="after" label="After">

```json title="package.json"
{
  "workspaces": [
    ".wasp/out/*",
    // highlight-next-line
    ".wasp/out/sdk/wasp"
  ],
  "dependencies": {
    // highlight-next-line
    "react-router": "^7.12.0",
    // ... other dependencies ...
  }
  // ... other fields ...
}
```

</TabItem>
</Tabs>

Now, we will clean up the old `.wasp` directory (to avoid any potential conflicts), and let `npm` pick up the new workspace configuration. You can do this by running the following command in your terminal:

```sh
wasp clean
wasp ts-setup # if you're using the TS Spec
wasp compile
```

### 4. Update React Router imports

We've upgraded from React Router 6 to React Router 7. The package has been renamed from `react-router-dom` to `react-router`, so you'll need to update your imports.

Search your codebase for the string `react-router-dom` and update imports to `react-router`:

<Tabs>
<TabItem value="before" label="Before">

```tsx title="src/SomePage.tsx"
// highlight-next-line
import { useNavigate, useParams } from 'react-router-dom'

// ...
```

</TabItem>
<TabItem value="after" label="After">

```tsx title="src/SomePage.tsx"
// highlight-next-line
import { useNavigate, useParams } from 'react-router'

// ...
```

</TabItem>
</Tabs>

React Router v7 is largely backwards compatible with v6, so there shouldn't be any changes besides the name.
For advanced usage, check the [React Router v6 to v7 upgrade guide](https://reactrouter.com/upgrading/v6).

### 5. Upgrade Tailwind CSS to v4

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


If you hit any snags or would like more details, check out the official [Tailwind CSS v4 upgrade guide](https://tailwindcss.com/docs/upgrade-guide), and our updated [Tailwind documentation](../project/css-frameworks.md#tailwind).

### 6. Update your custom Dockerfile

**If you don't have a `Dockerfile` in your project folder, you can skip this step.**

If you have a custom `Dockerfile` in your project, you need to update it to reference the new `.wasp/out` directory instead of the removed `.wasp/build` directory.

This can be a quite straightforward find-and-replace operation:
- **Find** `.wasp/build`
- **Replace** with `.wasp/out`

### 7. Update your custom deployment scripts

**If you use `wasp deploy fly` or `wasp deploy railway` to deploy your app, you can skip this step.**

If you have custom deployment scripts, you'll need to make two changes:

#### 1. Replace `.wasp/build` with `.wasp/out`

Since the `.wasp/build` directory has been removed, you need to update your scripts to reference `.wasp/out` instead:
- **Find** `.wasp/build`
- **Replace** with `.wasp/out`

#### 2. Update the client build command

Since Vite now runs from the project root, the way you build the client app for deployment has changed.

<Tabs>
<TabItem value="before" label="Before">

```shell
cd .wasp/out/web-app
npm install && REACT_APP_API_URL=<url_to_wasp_backend> npm run build
```

</TabItem>
<TabItem value="after" label="After">

```shell
REACT_APP_API_URL=<url_to_wasp_backend> npx vite build
```

Run this from the **project root**. The client build output directory stayed the same: `.wasp/out/web-app/build`.

</TabItem>
</Tabs>

You can check our updated [deployment methods guide](../deployment/deployment-methods/overview.md) and [CI/CD guide](../deployment/ci-cd.md) for reference on the correct deployment steps.

### 8. Upgrade Vitest tests to v4

**If you don't have test files in your project, you can skip this step.**

We upgraded our testing support from Vitest v1 to Vitest v4. Most of the breaking changes are related to internal configuration, edge cases, or very advanced usage; so we recommend **first to try running your tests after bumping the Wasp version**, and only read through the migration guides if you encounter issues:

1. [Migration guide from Vitest v1 to v2](https://v3.vitest.dev/guide/migration.html#vitest-2)
2. [Migration guide from Vitest v2 to v3](https://v3.vitest.dev/guide/migration.html#vitest-3)
3. [Migration guide from Vitest v3 to v4](https://vitest.dev/guide/migration.html#vitest-4)

### 9. Update custom server URL usage in deployment

**If you weren't using `REACT_APP_API_URL` environment variable during deployment, you can skip this step.**

If you were using the `REACT_APP_API_URL` environment variable to specify a custom server URL during deployment, you now need to use the `--custom-server-url` CLI option instead:

<Tabs>
<TabItem value="before" label="Before">

```sh
# For Fly
REACT_APP_API_URL=https://my-server.com wasp deploy fly launch ...
# For Railway
REACT_APP_API_URL=https://my-server.com wasp deploy railway launch ...
```

</TabItem>
<TabItem value="after" label="After">

```sh
# For Fly
wasp deploy fly launch --custom-server-url https://my-server.com ...
# For Railway
wasp deploy railway launch --custom-server-url https://my-server.com ...
```

</TabItem>
</Tabs>

### 10. Create `public/manifest.json`

**Skip this step if you don't care about PWA support**

Wasp no longer generates `manifest.json` automatically. If you want to enable PWA support, you'll need to create this file manually.

1. Create `manifest.json` in your project's `public/` directory:

    ```json title="public/manifest.json"
    {
      "name": "MyAwesomeApp",
      "icons": [
        {
          "src": "favicon.ico",
          "sizes": "64x64 32x32 24x24 16x16",
          "type": "image/x-icon"
        }
      ],
      "start_url": ".",
      "display": "standalone",
      "theme_color": "#000000",
      "background_color": "#ffffff"
    }
    ```

    Make sure to customize the `name`, `theme_color`, and `background_color` to match your app.

2. Add a link to the manifest in your head in your Wasp file:

    ```wasp title="main.wasp"
    app TodoApp {
      // ...
      head: [
        // highlight-next-line
        "<link rel='manifest' href='/manifest.json' />",
      ],
    }
    ```

### 11. Add `netlify.toml` if deploying to Netlify

**If you're not deploying to Netlify, you can skip this step.**

Wasp no longer generates a `netlify.toml` file in your project. If you're deploying to Netlify, you'll need to create this file manually in your project root.

Create a `netlify.toml` file with the following content:

<NetlifyTomlConfig />

For more details, see the [Netlify deployment documentation](../deployment/deployment-methods/paas.md#netlify).

### 12. Enjoy your updated Wasp app

That's it!
