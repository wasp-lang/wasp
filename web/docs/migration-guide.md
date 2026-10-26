---
sidebar_label: From 0.26 to 0.27
---

# Migration from 0.26 to 0.27

import InstallInstructions from './_install-instructions.md'

<InstallInstructions version="0.27" />

## What's new in 0.27?

### Prisma 6

Wasp now uses Prisma 6. Check out Prisma's [upgrade guide](https://www.prisma.io/docs/orm/more/upgrade-guides/upgrading-versions/upgrading-to-prisma-6) for the full list of changes.

### Smaller Docker images

The server's Docker image now contains only the files the server needs at runtime, instead of the whole `node_modules` folder. This makes the image about a third of the size.

The build steps are also ordered so that a code change reuses the installed dependencies, which makes rebuilding the image after a code change faster.

## How to migrate?

### 1. Bump the Wasp version

Update the version field in your Wasp config to `^0.27.0`.

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```ts title="main.wasp.ts"
    export default app({
      // highlight-next-line
      wasp: { version: "^0.26.0" },
      // ...
    });
    ```
  </TabItem>
  <TabItem value="after" label="After">
    ```ts title="main.wasp.ts"
    export default app({
      // highlight-next-line
      wasp: { version: "^0.27.0" },
      // ...
    });
    ```
  </TabItem>
</Tabs>

And run the following command to update the Wasp libraries in your project:

```bash
wasp install
```

### 2. Upgrade to Prisma 6

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

If you also list `@prisma/client` in your `package.json`, update it to the same version. Then run `wasp install`.

Prisma 6 changed how some things are stored in the database, so run `wasp db migrate-dev` to check if your app needs a new migration. Prisma 6 also has a few breaking changes in Prisma Client. Check [Prisma's upgrade guide](https://www.prisma.io/docs/orm/more/upgrade-guides/upgrading-versions/upgrading-to-prisma-6) to see if any of them affect your app.

### 3. Check the packages your server loads at runtime

**If your server doesn't use packages that serve or load files from their own folder, like `swagger-ui-express`, you can skip this step.**

The Docker image now contains only the files Wasp finds by following the server's imports. A package that loads files another way misses them in the image. A common case is a package that serves static assets from its own folder (e.g. `swagger-ui-express`), which would fail in production.

List such packages in [`server.traceDeps`](./deployment/methods/overview#missing-files-in-the-docker-image) to copy all of their files:

```ts title="main.wasp.ts"
export default app({
  // ...
  server: {
    traceDeps: ["swagger-ui-dist*"],
  },
})
```

### 4. Install Python if your app needs it

**If your app doesn't run Python on the server, you can skip this step.**

The generated Dockerfile no longer installs `python3` in the final image, because the Wasp server doesn't use it. If your app runs Python, install it in a [custom Dockerfile](./deployment/methods/overview#customizing-the-dockerfile):

```dockerfile title="Dockerfile"
RUN apk add --no-cache python3
```

### 5. Update your custom Dockerfile

**If you don't use a [custom Dockerfile](./deployment/methods/overview#customizing-the-dockerfile), you can skip this step.**

The generated Dockerfile changed. Run `wasp dockerfile` to print it with your custom Dockerfile appended at the bottom, and check that your steps still apply to the new stages.

The final stage (`server-production`) no longer contains the `/app/node_modules` folder. It only has the server's bundle and the packages the bundle uses, in `/app/.wasp/out/server`. If your steps use other packages (for example, through `npx`), copy the full `node_modules` from the `server-builder` stage:

```dockerfile title="Dockerfile"
COPY --from=server-builder /app/node_modules /app/node_modules
```

### 6. Enjoy your updated Wasp app

That's it!
