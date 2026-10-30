---
sidebar_label: From 0.26 to 0.27
---

# Migration from 0.26 to 0.27

## What's new in 0.27?

### Wasp as a project dependency

Your project now depends on Wasp through the `@wasp/wasp` npm package, which brings the Wasp CLI and the Wasp Spec at the version your `package.json` declares. The `wasp install` command and the `wasp: { version }` field in `main.wasp.ts` are gone. `npm install` installs everything, and `package.json` is the one place that says which Wasp version the project uses. The `wasp` command you install globally keeps working: inside a project, it runs that project's version.

### New npm organization

Wasp's npm packages moved from the `@wasp.sh` organization to `@wasp`. The Wasp CLI is now published as `@wasp/cli`, and the Wasp Spec is `@wasp/spec`.

### Prisma 6

Wasp now uses Prisma 6. Check out Prisma's [upgrade guide](https://www.prisma.io/docs/orm/more/upgrade-guides/upgrading-versions/upgrading-to-prisma-6) for the full list of changes.

### Smaller Docker images

The server's Docker image now contains only the files the server needs at runtime, instead of the whole `node_modules` folder. This makes the image about half the size.

Wasp finds those files by analyzing the server's imports. Some packages load their files in a non-standard way that this analysis can't follow, and you might need to list them in `server.traceDeps`.

The build steps are also ordered so that a code change reuses the installed dependencies, which makes rebuilding the image after a code change faster.

## How to migrate?

### 1. Switch to the new Wasp CLI package

The Wasp CLI is now published as `@wasp/cli` instead of `@wasp.sh/wasp-cli`. Uninstall the old package first, otherwise npm fails with an `EEXIST` error when installing the new one:

```sh
npm uninstall -g @wasp.sh/wasp-cli
npm i -g @wasp/cli@0.27
```

### 2. Depend on `@wasp/wasp` instead of `@wasp.sh/spec`

Replace the `@wasp.sh/spec` devDependency with the `@wasp/wasp` dependency, pinned to the exact Wasp version. The `wasp` script is optional, but it lets you run Wasp through the project (`npm run wasp start`, with `--` before any flags) without a global install:

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

Your project's `.npmrc` keeps npm from installing packages published less than 7 days ago. Wasp's own packages are published with each release, so exempt them, otherwise the install below fails until the release is a week old:

```ini title=".npmrc"
min-release-age=7
# highlight-next-line
min-release-age-exclude[]=@wasp/*
```

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

Update the Prisma packages in your `package.json`:

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```json title="package.json"
    {
      "dependencies": {
        // highlight-next-line
        "@prisma/client": "5.19.1" // if already present
      },
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
      "dependencies": {
        // highlight-next-line
        "@prisma/client": "6.19.3" // if already present
      },
      "devDependencies": {
        // highlight-next-line
        "prisma": "6.19.3"
      }
    }
    ```
  </TabItem>
</Tabs>

Then start from a clean state, install the dependencies, which now include Wasp, and migrate. This is the first install after the changes above on purpose. `@wasp/wasp` requires Prisma 6, and installing before this step fails with a dependency conflict.

```sh
wasp clean
npm install
wasp db migrate-dev
```

Prisma 6 changed how some things are stored in the database, so `wasp db migrate-dev` creates a new migration if your app needs one. Prisma 6 also has a few breaking changes in Prisma Client. Check [Prisma's upgrade guide](https://www.prisma.io/docs/orm/more/upgrade-guides/upgrading-versions/upgrading-to-prisma-6) to see if any of them affect your app.

### 5. Check the packages your server loads at runtime

**If you didn't add any npm packages to your app yourself, you can skip this step.** The packages Wasp uses work out of the box.

Otherwise, check if any of the packages your server uses serve or load files from their own folder, like `swagger-ui-express`.

The Docker image now contains only the files Wasp finds by following the server's imports. A package that loads files in a non-standard way misses them in the image. A common case is a package that serves static assets from its own folder (e.g. `swagger-ui-express`), which would fail in production.

List such packages in [`server.traceDeps`](./deployment/methods/overview#missing-files-in-the-docker-image) to copy all of their files:

```ts title="main.wasp.ts"
export default app({
  // ...
  server: {
    traceDeps: ["swagger-ui-dist*"],
  },
})
```

### 6. Install the system dependencies your app needs

**If your app doesn't run Python on the server, you can skip this step.**

The generated Dockerfile no longer installs `python3` in the final image, because the Wasp server doesn't use it. If your app runs Python, install it in a [custom Dockerfile](./deployment/methods/overview#customizing-the-dockerfile):

```dockerfile title="Dockerfile"
RUN apk add --no-cache python3
```

### 7. Update your custom Dockerfile

**If you don't have a [`Dockerfile`](./deployment/methods/overview#customizing-the-dockerfile) in your project's dir, you can skip this step.**

The generated Dockerfile changed. Run `wasp dockerfile` to print it with your custom Dockerfile appended at the bottom, and check that your steps still apply to the new stages.

The final stage (`server-production`) no longer contains the `/app/node_modules` folder. It only has the server's bundle and the packages the bundle uses, in `/app/.wasp/out/server`. If your steps use other packages (for example, through `npx`), copy the full `node_modules` from the `server-builder` stage:

```dockerfile title="Dockerfile"
COPY --from=server-builder /app/node_modules /app/node_modules
```

### 8. Enjoy your updated Wasp app

That's it!
