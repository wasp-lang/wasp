---
sidebar_label: From 0.26 to 0.27
---

# Migration from 0.26 to 0.27

## What's new in 0.27?

### Wasp as a project dependency

Your project now depends on Wasp through the `@wasp/wasp` npm package, which brings the Wasp CLI and the Wasp Spec at the version your `package.json` declares. The `wasp install` command and the `wasp: { version }` field in `main.wasp.ts` are gone: `npm install` installs everything, and `package.json` is the one place that says which Wasp version the project uses. The `wasp` command you install globally keeps working: inside a project, it runs that project's version.

### New npm organization

Wasp's npm packages moved from the `@wasp.sh` organization to `@wasp`. The Wasp CLI is now published as `@wasp/cli`, and the Wasp Spec is `@wasp/spec`.

### Prisma 6

Wasp now uses Prisma 6. Check out Prisma's [upgrade guide](https://www.prisma.io/docs/orm/more/upgrade-guides/upgrading-versions/upgrading-to-prisma-6) for the full list of changes.

## How to migrate?

### 1. Switch to the new Wasp CLI package

The Wasp CLI is now published as `@wasp/cli` instead of `@wasp.sh/wasp-cli`. Uninstall the old package first, otherwise npm fails with an `EEXIST` error when installing the new one:

```sh
npm uninstall -g @wasp.sh/wasp-cli
npm i -g @wasp/cli@0.27
```

### 2. Depend on `@wasp/wasp` instead of `@wasp.sh/spec`

Replace the `@wasp.sh/spec` devDependency with the `@wasp/wasp` dependency, pinned to the exact Wasp version. The `wasp` script is optional, but it lets you run Wasp through the project (`npm run wasp start`) without a global install:

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```json title="package.json"
    {
      "devDependencies": {
        // highlight-next-line
        "@wasp.sh/spec": "file:.wasp/spec"
      }
    }
    ```
  </TabItem>
  <TabItem value="after" label="After">
    ```json title="package.json"
    {
      // highlight-start
      "scripts": {
        "wasp": "wasp"
      },
      "dependencies": {
        "@wasp/wasp": "0.27.0"
      }
      // highlight-end
    }
    ```
  </TabItem>
</Tabs>

The Wasp Spec package is now `@wasp/spec`, so update the imports in your `main.wasp.ts` and any other `*.wasp.ts` files:

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```ts title="main.wasp.ts"
    // highlight-next-line
    import { app, page, route } from "@wasp.sh/spec";
    ```
  </TabItem>
  <TabItem value="after" label="After">
    ```ts title="main.wasp.ts"
    // highlight-next-line
    import { app, page, route } from "@wasp/spec";
    ```
  </TabItem>
</Tabs>

### 3. Remove the Wasp version from `main.wasp.ts`

The version in `package.json` is now the one that counts, so the `wasp` field goes away:

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```ts title="main.wasp.ts"
    export default app({
      name: "myApp",
      // highlight-next-line
      wasp: { version: "^0.26.0" },
      // ...
    });
    ```
  </TabItem>
  <TabItem value="after" label="After">
    ```ts title="main.wasp.ts"
    export default app({
      name: "myApp",
      // ...
    });
    ```
  </TabItem>
</Tabs>

### 4. Upgrade to Prisma 6

Update the `prisma` version in your `package.json`:

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```json title="package.json"
    {
      "devDependencies": {
        // highlight-next-line
        "prisma": "5.19.1"
      }
    }
    ```
  </TabItem>
  <TabItem value="after" label="After">
    ```json title="package.json"
    {
      "devDependencies": {
        // highlight-next-line
        "prisma": "6.19.3"
      }
    }
    ```
  </TabItem>
</Tabs>

If you also list `@prisma/client` in your `package.json`, update it to the same version.

Then start from a clean state and install the dependencies, which now include Wasp. This is the first install after the changes above on purpose: `@wasp/wasp` requires Prisma 6, so installing before this step fails with a dependency conflict.

```bash
wasp clean
npm install
```

Prisma 6 changed how some things are stored in the database, so run `wasp db migrate-dev` to check if your app needs a new migration. Prisma 6 also has a few breaking changes in Prisma Client. Check [Prisma's upgrade guide](https://www.prisma.io/docs/orm/more/upgrade-guides/upgrading-versions/upgrading-to-prisma-6) to see if any of them affect your app.

### 5. Enjoy your updated Wasp app

That's it!
