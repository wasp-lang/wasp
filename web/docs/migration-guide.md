---
title: From 0.24 to 0.25
---

# Migration from 0.24 to 0.25

import InstallInstructions from './\_install-instructions.md'

<InstallInstructions version="0.25" />

## What's new in 0.25?

### TypeScript 6

Wasp now uses **TypeScript 6**. Your projects will be built with TypeScript `6.0.3`. Since Wasp runs on Node 24+, `tsconfig.wasp.json`'s we also bumped the `target` and `lib` options to `ES2025`.

### React Router 8

Wasp now uses **React Router 8**. The upgrade is backwards compatible for Wasp users.

### Vite 8

Wasp now uses **Vite 8**, which is powered by a new native bundler, for faster builds. Our testing support is also bumped to **Vitest 4.1**, to stay compatible.

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

And run the following command to update the Wasp libraries in your project:

```bash
wasp install
```

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
        "@tailwindcss/vite": "^4.1.18", // only if already present
        "typescript": "5.9.3",
        "vite": "^7.0.6",
        "vitest": "^4.0.16"
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
        "@tailwindcss/vite": "^4.3.1", // only if already present
        "typescript": "6.0.3",
        "vite": "^8.1.0",
        "vitest": "^4.1.9"
      }
    }
    ```
  </TabItem>
</Tabs>

### 3. Update your TypeScript config for TypeScript 6

TypeScript 6 no longer automatically includes `@types/*` packages, so you must list the required type packages explicitly. In `tsconfig.wasp.json`, also bump `target` and `lib` to `ES2025`.

In `tsconfig.wasp.json`:

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
        "target": "ES2025",
        "lib": ["ES2025"],
        "types": ["node"]
      }
    }
    ```
  </TabItem>
</Tabs>

In `tsconfig.src.json`:

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```json title="tsconfig.src.json"
    {
      "compilerOptions": {
        // ...
      }
    }
    ```
  </TabItem>
  <TabItem value="after" label="After">
    ```json title="tsconfig.src.json"
    {
      "compilerOptions": {
        // ...
        "types": ["react", "node"]
      }
    }
    ```
  </TabItem>
</Tabs>

### 4. Enjoy your updated Wasp app

That's it!
