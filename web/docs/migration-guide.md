---
title: From 0.24 to 0.25
---

# Migration from 0.24 to 0.25

import InstallInstructions from './\_install-instructions.md'

<InstallInstructions version="0.25" />

## What's new in 0.25?

### TypeScript 6

Wasp now uses **TypeScript 6**. Your projects will be built with TypeScript `6.0.3`, and your project's TypeScript config files need a couple of small updates (see below). Since Wasp runs on Node 24+, `tsconfig.wasp.json`'s `target` and `lib` were also bumped to `ES2025`.

### React Router 8

Wasp now uses **React Router 8**. The upgrade is backwards compatible for typical usage (e.g. `Link`, `NavLink`, `Outlet`), so you only need to bump the `react-router` dependency.

### `app.head` is now JSX

The `head` field of your `app` spec now takes JSX elements instead of HTML strings. Since JSX is only allowed in `.tsx` files, spec files that use it must be named `main.wasp.tsx`. The required `tsconfig.wasp.json` and `tsconfig.src.json` settings changed accordingly (see below).

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

### 2. Update your dependencies in `package.json`

Bump Wasp-required dependencies to their latest version:

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```json title="package.json"
    {
      "dependencies": {
        // ...
        "react-router": "^7.12.0"
      },
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
      "dependencies": {
        // ...
        "react-router": "^8.0.1"
      },
      "devDependencies": {
        // ...
        "typescript": "6.0.3"
      }
    }
    ```
  </TabItem>
</Tabs>

### 3. Convert `app.head` to JSX

If your app doesn't set `head`, skip this step.

Rename `main.wasp.ts` to `main.wasp.tsx` and rewrite the `head` strings as JSX elements:

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```ts title="main.wasp.ts"
    export default app({
      // ...
      // highlight-next-line
      head: ["<link rel='icon' href='/favicon.ico' />"],
    });
    ```
  </TabItem>
  <TabItem value="after" label="After">
    ```tsx title="main.wasp.tsx"
    export default app({
      // ...
      // highlight-next-line
      head: [<link rel="icon" href="/favicon.ico" />],
    });
    ```
  </TabItem>
</Tabs>

Only plain HTML elements with serializable props are supported — components and event handlers (like `onLoad`) are not.

### 4. Update your TypeScript configs

TypeScript 6 no longer automatically includes `@types/*` packages, so you must list the required type packages explicitly. In `tsconfig.wasp.json`, also bump `target` and `lib` to `ES2025`, switch `jsx` to `react-jsx` (needed to typecheck JSX in `main.wasp.tsx`), and add the `**/*.wasp.tsx` glob to `include`.

In `tsconfig.wasp.json`:

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```json title="tsconfig.wasp.json"
    {
      "compilerOptions": {
        // ...
        "target": "ES2022",
        "lib": ["ES2023"],
        "jsx": "preserve"
      },
      "include": ["**/*.wasp.ts", ".wasp/out/types/spec"]
    }
    ```
  </TabItem>
  <TabItem value="after" label="After">
    ```json title="tsconfig.wasp.json"
    {
      "compilerOptions": {
        // ...
        "target": "ES2025",
        "lib": ["ES2025"],
        "jsx": "react-jsx",
        "types": ["node"]
      },
      "include": ["**/*.wasp.ts", "**/*.wasp.tsx", ".wasp/out/types/spec"]
    }
    ```
  </TabItem>
</Tabs>

In `tsconfig.src.json`, add the `**/*.wasp.tsx` glob to `exclude`:

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```json title="tsconfig.src.json"
    {
      "compilerOptions": {
        // ...
        "outDir": ".wasp/out/user"
      },
      "exclude": ["**/*.wasp.ts"]
    }
    ```
  </TabItem>
  <TabItem value="after" label="After">
    ```json title="tsconfig.src.json"
    {
      "compilerOptions": {
        // ...
        "outDir": ".wasp/out/user",
        "types": ["react", "node"]
      },
      "exclude": ["**/*.wasp.ts", "**/*.wasp.tsx"]
    }
    ```
  </TabItem>
</Tabs>

### 5. Enjoy your updated Wasp app

That's it!
