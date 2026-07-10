---
title: From 0.24 to 0.25
---

# Migration from 0.24 to 0.25

import InstallInstructions from './\_install-instructions.md'

<InstallInstructions version="0.25" />

## What's new in 0.25?

### TypeScript 6

Wasp now uses **TypeScript 6**. Your projects will be built with TypeScript `6.0.3`, and your project's TypeScript config files need a couple of small updates (see below). Since Wasp runs on Node 24+, `tsconfig.wasp.json`'s `target` and `lib` were also bumped to `ES2025`.

## How to migrate?

### 1. Bump the Wasp version

Update the version field in your Wasp config to `^0.25.0`.

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```ts title="main.wasp.ts"
    export default app({
      // highlight-next-line
      wasp: { version: "^0.24.0" },
      // ...
    });
    ```
  </TabItem>
  <TabItem value="after" label="After">
    ```ts title="main.wasp.ts"
    export default app({
      // highlight-next-line
      wasp: { version: "^0.25.0" },
      // ...
    });
    ```
  </TabItem>
</Tabs>

### 2. Update your TypeScript configuration

Due to TypeScript 6 upgrade and internal `wasp/sdk` package changes, we require some changes to your TypeScript configuration.

TypeScript 6 no longer automatically includes `@types/*` packages, so you must list the required type packages explicitly.

In `tsconfig.wasp.json`, list the types and bump `target` and `lib` to `ES2025`:

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```json title="tsconfig.wasp.json"
    {
      "compilerOptions": {
        // ...
        "target": "ES2022",
        "lib": ["ES2023"]
      }
    }
    ```
  </TabItem>
  <TabItem value="after" label="After">
    ```json title="tsconfig.wasp.json"
    {
      "compilerOptions": {
        // ...
        // highlight-start
        "target": "ES2025",
        "lib": ["ES2025"],
        "types": ["node"]
        // highlight-end
      }
    }
    ```
  </TabItem>
</Tabs>

In `tsconfig.src.json`, list the types and update the `include` field:

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```json title="tsconfig.src.json"
    {
      "compilerOptions": {
        // ...
        "outDir": ".wasp/out/user"
      }
      "include": ["src"]
    }
    ```
  </TabItem>
  <TabItem value="after" label="After">
    ```json title="tsconfig.src.json"
    {
      "compilerOptions": {
        // ...
        "outDir": ".wasp/out/user",
        // highlight-next-line
        "types": ["react", "node"]
      }  
      // highlight-next-line
      "include": ["src", ".wasp/out/types/sdk"]
    }
    ```
  </TabItem>
</Tabs>

In `package.json`, update the `typescript` dev dependency to `6.0.3`:

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```json title="package.json"
    {
      "devDependencies": {
        // ...
        "typescript": "5.9.3"
      }
    }
    ```
  </TabItem>
  <TabItem value="after" label="After">
    ```json title="package.json"
    {
      "devDependencies": {
        // ...
        // highlight-next-line
        "typescript": "6.0.3"
      }
    }
    ```
  </TabItem>
</Tabs>

### 3. Enjoy your updated Wasp app

That's it!
