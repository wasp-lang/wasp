---
sidebar_label: From 0.26 to 0.27
---

# Migration from 0.26 to 0.27

import InstallInstructions from './_install-instructions.md'

<InstallInstructions version="0.27" />

## What's new in 0.27?

### Prisma 6

Wasp now uses Prisma 6. Check out Prisma's [upgrade guide](https://www.prisma.io/docs/orm/more/upgrade-guides/upgrading-versions/upgrading-to-prisma-6) for the full list of changes.

### Testing with your own Vitest setup

Wasp no longer installs and runs [Vitest](https://vitest.dev/) for you. You set it up like in any other Vite app, so you choose the versions of Vitest and the testing libraries.

The `wasp/client/test` helpers for rendering components and mocking queries and APIs still work. Read more in the [testing docs](./advanced/testing.md).

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

### 3. Update your Vitest setup

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

2. Add the options Wasp used to set for you to your `vite.config.ts`:

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

       export default defineConfig({
         plugins: [wasp()],
         // highlight-start
         test: {
           globals: true,
           environment: "jsdom",
           setupFiles: ["./src/test/setup.ts"],
           include: ["src/**/*.{test,spec}.?(c|m)[jt]s?(x)"],
         },
         // highlight-end
       });
       ```
     </TabItem>
   </Tabs>

3. Create the setup file you referenced in `setupFiles`:

   ```ts title="src/test/setup.ts"
   import "@testing-library/jest-dom/vitest";
   import { cleanup } from "@testing-library/react";
   import { afterEach } from "vitest";

   afterEach(() => {
     cleanup();
   });
   ```

4. Run your tests with Vitest instead of `wasp test client`. Your tests import the Wasp SDK, so generate it first with `wasp compile`, or keep `wasp start` running in another terminal:

   ```bash
   wasp compile
   npx vitest
   ```

Read more in the [testing docs](./advanced/testing.md).

### 4. Enjoy your updated Wasp app

That's it!
