---
sidebar_label: From 0.26 to 0.27
---

# Migration from 0.26 to 0.27

## What's new in 0.27?

### Node.js 26

Wasp now requires Node.js 26 or later, the [current LTS line](https://github.com/nodejs/release#release-schedule). It enables the [Temporal API](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal) by default and removes some long-deprecated APIs. Check out Node.js's [release notes](https://nodejs.org/en/blog/release/v26.0.0) for the full list of changes.

### New npm organization

Wasp's npm packages moved from the `@wasp.sh` organization to `@wasp`. The Wasp CLI is now published as `@wasp/cli`, and the Wasp Spec is `@wasp/spec`.

### Prisma 6

Wasp now uses Prisma 6. Check out Prisma's [upgrade guide](https://www.prisma.io/docs/orm/more/upgrade-guides/upgrading-versions/upgrading-to-prisma-6) for the full list of changes.

### Testing with your own Vitest setup

Wasp no longer installs and runs [Vitest](https://vitest.dev/) for you. You set it up like in any other Vite app, so you choose the versions of Vitest and the testing libraries.

The `wasp/client/test` helpers for rendering components and mocking queries and APIs still work. Read more in the [testing docs](./advanced/testing.md).

### Smaller Docker images

The server's Docker image now contains only the files the server needs at runtime, instead of the whole `node_modules` folder. This makes the image about half the size.

Wasp finds those files by analyzing the server's imports. Some packages load their files in a non-standard way that this analysis can't follow, and you might need to list them in `server.traceDeps`.

The build steps are also ordered so that a code change reuses the installed dependencies, which makes rebuilding the image after a code change faster.

## How to migrate?

### 1. Upgrade Node.js to 26.13.1 or later {#upgrade-nodejs}

Wasp 0.27 requires Node.js >= 26.13.1 (previously >= 24.22.1). The new Wasp CLI refuses to run on older versions, so upgrade before continuing:

```shell
node -v
# If below 26.13.1, upgrade:
nvm install 26
```

If you deploy with the Dockerfile Wasp generates, your server image moves to `node:26.13.1-alpine3.23` when you rebuild it. If you use a custom Dockerfile, a CI workflow, or a platform that picks its own Node.js version, make sure it runs 26.13.1 or newer.

Wasp also requires `@types/node` to match the Node.js major version. Update it in your `package.json`:

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```json title="package.json"
    {
      "devDependencies": {
        // highlight-next-line
        "@types/node": "^24.0.0"
      }
    }
    ```
  </TabItem>
  <TabItem value="after" label="After">
    ```json title="package.json"
    {
      "devDependencies": {
        // highlight-next-line
        "@types/node": "^26.0.0"
      }
    }
    ```
  </TabItem>
</Tabs>

Node.js 26 is a new major version with breaking changes. Check out Node.js's [release notes](https://nodejs.org/en/blog/release/v26.0.0) to see if any of them affect your app.

### 2. Switch to the new Wasp CLI package

The Wasp CLI is now published as `@wasp/cli` instead of `@wasp.sh/wasp-cli`. Uninstall the old package first, otherwise npm fails with an `EEXIST` error when installing the new one:

```sh
npm uninstall -g @wasp.sh/wasp-cli
npm i -g @wasp/cli@0.27
```

### 3. Rename `@wasp.sh/spec` to `@wasp/spec`

Wasp's packages moved from the `@wasp.sh` npm scope to `@wasp`. Rename the Wasp Spec dependency in your `package.json`:

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
      "devDependencies": {
        // highlight-next-line
        "@wasp/spec": "file:.wasp/spec"
      }
    }
    ```
  </TabItem>
</Tabs>

Then replace `@wasp.sh/spec` with `@wasp/spec` everywhere else in your project. That covers the imports in `main.wasp.ts` and any other `*.wasp.ts` files, and mentions in files like `AGENTS.md` or `CLAUDE.md`:

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

### 4. Bump the Wasp version

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

### 5. Update your Vitest setup

The `wasp test` command is gone, and Wasp no longer installs testing libraries or configures Vitest for you. The `wasp/client/test` helpers (`renderInContext` and `mockServer`) stay, but they now need `vitest`, `msw`, and `@testing-library/react` in your project's `devDependencies`.

Follow the part that matches your app.

#### If you don't use `wasp test client`

Wasp no longer needs `vitest`, so you can remove it from your project:

1. If your `vite.config.ts` imports `defineConfig` from `vitest/config`, import it from `vite` instead, and remove the `test` section. Otherwise, Vite can't load your config after you remove `vitest`.

   <Tabs sideBySide>
     <TabItem value="before" label="Before">
       ```ts title="vite.config.ts"
       // highlight-next-line
       import { defineConfig } from "vitest/config";
       import { wasp } from "wasp/client/vite";

       export default defineConfig({
         plugins: [wasp()],
         // highlight-start
         test: {
           exclude: ["./e2e-tests/**"],
         },
         // highlight-end
       });
       ```
     </TabItem>
     <TabItem value="after" label="After">
       ```ts title="vite.config.ts"
       // highlight-next-line
       import { defineConfig } from "vite";
       import { wasp } from "wasp/client/vite";

       export default defineConfig({
         plugins: [wasp()],
       });
       ```
     </TabItem>
   </Tabs>

2. Remove `vitest` from your `devDependencies`:

   <Tabs sideBySide>
     <TabItem value="before" label="Before">
       ```json title="package.json"
       {
         "devDependencies": {
           // highlight-next-line
           "vitest": "^4.1.9",
           "vite": "^8.1.0"
         }
       }
       ```
     </TabItem>
     <TabItem value="after" label="After">
       ```json title="package.json"
       {
         "devDependencies": {
           "vite": "^8.1.0"
         }
       }
       ```
     </TabItem>
   </Tabs>

3. Remove the `vitest/globals` and `@testing-library/jest-dom` references from `src/vite-env.d.ts`, if you have them:

   <Tabs sideBySide>
     <TabItem value="before" label="Before">
       ```ts title="src/vite-env.d.ts"
       /// <reference types="vite/client" />
       // highlight-start
       /// <reference types="vitest/globals" />
       /// <reference types="@testing-library/jest-dom" />
       // highlight-end
       ```
     </TabItem>
     <TabItem value="after" label="After">
       ```ts title="src/vite-env.d.ts"
       /// <reference types="vite/client" />
       ```
     </TabItem>
   </Tabs>

#### If you use `wasp test client`

1. Install Vitest and the testing libraries your tests use:

   ```bash
   npm install -D vitest jsdom @testing-library/react @testing-library/jest-dom msw
   ```

   Wasp's testing helpers need Vitest 5 and MSW 3, so this also upgrades Vitest if your project is on an older version. If your tests use Vitest features that changed, check [Vitest's migration guide](https://vitest.dev/guide/migration/).

2. Set up Vitest in your `vite.config.ts` with Wasp's `waspTestConfig` helper. It sets the options Wasp used to set for you:

   <Tabs sideBySide>
     <TabItem value="before" label="Before">
       ```ts title="vite.config.ts"
       import { defineConfig } from "vite";
       import { wasp } from "wasp/client/vite";

       export default defineConfig({
         plugins: [wasp()],
       });
       ```
     </TabItem>
     <TabItem value="after" label="After">
       ```ts title="vite.config.ts"
       // highlight-next-line
       import { defineConfig } from "vitest/config";
       import { wasp } from "wasp/client/vite";
       // highlight-next-line
       import { waspTestConfig } from "wasp/test";

       export default defineConfig({
         plugins: [wasp()],
         // highlight-start
         test: waspTestConfig({
           setupFiles: ["./src/test/setup.ts"],
         }),
         // highlight-end
       });
       ```
     </TabItem>
   </Tabs>

   Wasp used to make `test`, `expect`, and the other test functions available globally. If your tests use them without importing them, pass `globals: true` to `waspTestConfig`:

   <Tabs sideBySide>
     <TabItem value="before" label="Before">
       ```ts title="vite.config.ts"
       test: waspTestConfig({
         setupFiles: ["./src/test/setup.ts"],
       }),
       ```
     </TabItem>
     <TabItem value="after" label="After">
       ```ts title="vite.config.ts"
       test: waspTestConfig({
         setupFiles: ["./src/test/setup.ts"],
         // highlight-next-line
         globals: true,
       }),
       ```
     </TabItem>
   </Tabs>

   Keep the `vitest/globals` types in `src/vite-env.d.ts`, so TypeScript knows about the globals when Wasp type-checks your tests:

   ```ts title="src/vite-env.d.ts"
   /// <reference types="vite/client" />
   // highlight-next-line
   /// <reference types="vitest/globals" />
   ```

   Or import them from `vitest` in each test file instead:

   <Tabs sideBySide>
     <TabItem value="before" label="Before">
       ```ts title="src/utils.test.ts"
       import { add } from "./utils";

       test("adds numbers", () => {
         expect(add(1, 2)).toBe(3);
       });
       ```
     </TabItem>
     <TabItem value="after" label="After">
       ```ts title="src/utils.test.ts"
       // highlight-next-line
       import { expect, test } from "vitest";
       import { add } from "./utils";

       test("adds numbers", () => {
         expect(add(1, 2)).toBe(3);
       });
       ```
     </TabItem>
   </Tabs>


3. Create the setup file you referenced in `setupFiles`. It adds the DOM matchers, like `toBeInTheDocument()`, and removes what a test rendered when the test ends:

   ```ts title="src/test/setup.ts"
   import "@testing-library/jest-dom/vitest";
   import { cleanup } from "@testing-library/react";
   import { afterEach } from "vitest";

   afterEach(() => {
     cleanup();
   });
   ```

   The `import "@testing-library/jest-dom/vitest"` line also adds the types for the DOM matchers, so you can remove the `@testing-library/jest-dom` reference from `src/vite-env.d.ts`:

   <Tabs sideBySide>
     <TabItem value="before" label="Before">
       ```ts title="src/vite-env.d.ts"
       /// <reference types="vite/client" />
       /// <reference types="vitest/globals" />
       // highlight-next-line
       /// <reference types="@testing-library/jest-dom" />
       ```
     </TabItem>
     <TabItem value="after" label="After">
       ```ts title="src/vite-env.d.ts"
       /// <reference types="vite/client" />
       /// <reference types="vitest/globals" />
       ```
     </TabItem>
   </Tabs>

4. Run your tests with Vitest instead of `wasp test client`. Add a `test` script to your `package.json`, or change it if it runs `wasp test client`:

   <Tabs sideBySide>
     <TabItem value="before" label="Before">
       ```json title="package.json"
       {
         "scripts": {
           // highlight-next-line
           "test": "wasp test client"
         }
       }
       ```
     </TabItem>
     <TabItem value="after" label="After">
       ```json title="package.json"
       {
         "scripts": {
           // highlight-next-line
           "test": "vitest"
         }
       }
       ```
     </TabItem>
   </Tabs>

   Your tests import the Wasp SDK, so generate it first with `wasp compile`, or keep `wasp start` running in another terminal:

   ```bash
   wasp compile
   npm test
   ```

Read more in the [testing docs](./advanced/testing.md).

### 6. Upgrade to Prisma 6

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

Then run:

```sh
wasp install
wasp db migrate-dev
```

Prisma 6 changed how some things are stored in the database, so `wasp db migrate-dev` creates a new migration if your app needs one. Prisma 6 also has a few breaking changes in Prisma Client. Check [Prisma's upgrade guide](https://www.prisma.io/docs/orm/more/upgrade-guides/upgrading-versions/upgrading-to-prisma-6) to see if any of them affect your app.

### 7. Check relative paths in your server code

The server now runs with your project's root directory as its working directory, instead of `.wasp/out/server`. In the Docker image, that's `/app` instead of `/app/.wasp/out/server`.

If your server code reads or writes files using relative paths, they now resolve from the project root. If you worked around the old behavior (e.g. with paths like `../../../src/data/countries.json`), remove the workaround. If you deploy with Docker and mount a volume at a path inside `/app/.wasp/out/server`, move it to the matching path inside `/app`.

### 8. Check the packages your server loads at runtime

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

### 9. Install the system dependencies your app needs

**If your app doesn't run Python on the server, you can skip this step.**

The generated Dockerfile no longer installs `python3` in the final image, because the Wasp server doesn't use it. If your app runs Python, install it in a [custom Dockerfile](./deployment/methods/overview#customizing-the-dockerfile):

```dockerfile title="Dockerfile"
RUN apk add --no-cache python3
```

### 10. Update your custom Dockerfile

**If you don't have a [`Dockerfile`](./deployment/methods/overview#customizing-the-dockerfile) in your project's dir, you can skip this step.**

The generated Dockerfile changed. Run `wasp dockerfile` to print it with your custom Dockerfile appended at the bottom, and check that your steps still apply to the new stages.

The final stage (`server-production`) no longer contains the `/app/node_modules` folder. It only has the server's bundle and the packages the bundle uses, in `/app/.wasp/out/server`. If your steps use other packages (for example, through `npx`), copy the full `node_modules` from the `server-builder` stage:

```dockerfile title="Dockerfile"
COPY --from=server-builder /app/node_modules /app/node_modules
```

### 11. Enjoy your updated Wasp app

That's it!
