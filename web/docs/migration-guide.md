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

### Vite 8

Wasp now uses **Vite 8**, which is powered by a new native bundler, for faster builds. Testing moves to **Vitest 4.1** to stay compatible.

### Type-checked auth routes

Auth route references are now **type-checked**. The `onAuthFailedRedirectTo` and `onAuthSucceededRedirectTo` fields, and the email `emailVerification`/`passwordReset` `clientRoute` fields, now take a `route()` object instead of a path or route-name string. This guarantees every reference points at a route that actually exists. A referenced route that isn't already listed in `spec` is registered automatically, so you can define it in place without repeating it.

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
        "@tailwindcss/vite": "^4.1.18", // only if present
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
        "@tailwindcss/vite": "^4.3.1", // only if present
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
        "outDir": ".wasp/out/user"
      }
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
      }
    }
    ```
  </TabItem>
</Tabs>

### 4. Pass `route()` objects to your `auth` config

If your app uses `auth`, replace the string route references with the matching `route()` object. The redirect fields (`onAuthFailedRedirectTo`, `onAuthSucceededRedirectTo`) and the email `clientRoute` fields no longer accept strings. Since referenced routes are registered automatically, a route that was only listed in `spec` to be referenced here can be defined inline.

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```ts title="main.wasp.ts"
    export default app({
      // ...
      auth: {
        // ...
        methods: {
          email: {
            // ...
            emailVerification: {
              clientRoute: "EmailVerificationRoute",
            },
            passwordReset: {
              clientRoute: "PasswordResetRoute",
            },
          },
        },
        onAuthFailedRedirectTo: "/login",
        onAuthSucceededRedirectTo: "/",
      },
      spec: [
        route("LoginRoute", "/login", page(LoginPage)),
        route("HomeRoute", "/", page(HomePage)),
        route(
          "EmailVerificationRoute",
          "/email-verification",
          page(EmailVerificationPage),
        ),
        route("PasswordResetRoute", "/password-reset", page(PasswordResetPage)),
        // ...
      ],
    });
    ```
  </TabItem>
  <TabItem value="after" label="After">
    ```ts title="main.wasp.ts"
    // A route referenced in more than one place can be shared through a variable.
    const homeRoute = route("HomeRoute", "/", page(HomePage));

    export default app({
      // ...
      auth: {
        // ...
        methods: {
          email: {
            // ...
            emailVerification: {
              clientRoute: route(
                "EmailVerificationRoute",
                "/email-verification",
                page(EmailVerificationPage),
              ),
            },
            passwordReset: {
              clientRoute: route(
                "PasswordResetRoute",
                "/password-reset",
                page(PasswordResetPage),
              ),
            },
          },
        },
        onAuthFailedRedirectTo: route("LoginRoute", "/login", page(LoginPage)),
        onAuthSucceededRedirectTo: homeRoute,
      },
      // The routes referenced above are registered automatically, so they no
      // longer need to be listed here.
      spec: [
        homeRoute,
        // ...
      ],
    });
    ```
  </TabItem>
</Tabs>

### 5. Enjoy your updated Wasp app

That's it!
