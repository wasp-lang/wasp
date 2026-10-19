---
sidebar_label: From 0.26 to 0.27
---

# Migration from 0.26 to 0.27

import InstallInstructions from './_install-instructions.md'

<InstallInstructions version="0.27" />

## What's new in 0.27?

### Testing with your own Vitest setup

Wasp no longer runs your tests itself. You install and run [Vitest](https://vitest.dev/) like in any other Vite app, so you pick its version and the testing libraries you use.

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

### 2. Set up Vitest yourself

**If you don't have any tests, you can skip this step.** You can also remove `vitest` from your `devDependencies`, Wasp no longer needs it.

Wasp no longer runs your tests or installs testing libraries. The `wasp test` command is gone, and the `wasp/client/test` helpers (`renderInContext` and `mockServer`) now use the testing libraries you install. Wasp also no longer sets up Vitest for you, so you configure it yourself.

1. Install Vitest and the testing libraries your tests use:

   ```bash
   npm install -D vitest jsdom @testing-library/react @testing-library/jest-dom msw
   ```

2. Configure Vitest in your `vite.config.ts`. Wasp used to set these options for you:

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
           include: ["src/**/*.test.{ts,tsx}"],
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

4. Run your tests with Vitest instead of `wasp test client`. Your tests import the Wasp SDK, so generate it first with `wasp compile` (or keep `wasp start` running in another terminal):

   ```bash
   wasp compile
   npx vitest
   ```

Read more in the [testing docs](./advanced/testing.md).

### 3. Enjoy your updated Wasp app

That's it!
