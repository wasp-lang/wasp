---
title: Server-Side Rendering (SSR)
---

Wasp supports lightweight, per-page SSR aimed at public landing/marketing pages and basic SEO. This is **not** a full SSR framework (no dev‑mode SSR, no streaming, no server components, no authenticated SSR).

## Enable SSR on a page

```wasp title="main.wasp"
route LandingRoute { path: "/", to: LandingPage }
page LandingPage {
  component: import { Landing } from "@src/pages/Landing",
  ssr: true
}
```

**Rules**
- SSR pages must be **public** (`authRequired: true` is not allowed with `ssr: true`).
- Non‑SSR pages continue to work as normal (CSR).

## Head tags (title/meta/link)

Optionally export a `head` function from the page module. It is evaluated during SSR and injected into the HTML `<head>`.

```ts title="src/pages/Landing.tsx"
export const head = () => ({
  title: "Home",
  meta: [
    { name: "description", content: "Landing page" },
    { property: "og:title", content: "Home" },
  ],
  link: [
    { rel: "canonical", href: "https://example.com" },
  ],
});
```

**Notes**
- `head` is optional. If not provided, SSR uses the static `index.html` head.
- Keep it **fast and side‑effect free** (it runs on the server per request).
- If a page defines a `title` in its `head` export, it overrides the global `app.title`.

## 404 Pages

For catch-all routes (`path: "*"`), the server automatically returns HTTP 404 status. To make the 404 content visible without JavaScript (for SEO and accessibility), add `ssr: true`:

```wasp title="main.wasp"
route CatchAllRoute { path: "*", to: NotFoundPage }
page NotFoundPage {
  component: import { NotFoundPage } from "@src/pages/NotFoundPage",
  ssr: true  // Recommended for 404 pages
}
```

**Without `ssr: true`:** Status is 404, but content requires JavaScript to render.
**With `ssr: true`:** Status is 404 and content is server-rendered (visible to crawlers and users with JS disabled).

## Local preview (SSR)

SSR is only available in **production build** mode (not in `wasp start` dev mode).

### Quick start

1. Start the managed database (in a separate terminal):
   ```bash
   wasp db start
   ```
   Note the connection URL printed in the output (e.g., `postgresql://postgresWaspDevUser:postgresWaspDevPass@localhost:5432/YourApp-xxxxx`).

2. Generate the code:
   ```bash
   wasp build
   ```

3. Build and run with the database credentials:
   ```bash
   wasp build start \
     --server-env DATABASE_URL=postgresql://postgresWaspDevUser:postgresWaspDevPass@localhost:5432/YourApp-xxxxx \
     --server-env JWT_SECRET=dev-secret-change-me
   ```

4. Open `http://localhost:3000` (SSR client) and `http://localhost:3001` (API server).

### Alternative: Using .env.server

Instead of passing `--server-env` flags, you can add the credentials to your `.env.server` file:

```bash title=".env.server"
DATABASE_URL=postgresql://postgresWaspDevUser:postgresWaspDevPass@localhost:5432/YourApp-xxxxx
JWT_SECRET=dev-secret-change-me
```

Then run:
```bash
wasp build
wasp build start
```

:::caution
Remember to update the `DATABASE_URL` if you recreate the managed database, as the database name includes a unique hash.
:::

## CSS-in-JS Support (Emotion, styled-components, etc.)

When using CSS-in-JS libraries like **Emotion** (used by MUI), **styled-components**, or **Stitches** with SSR, styles need to be extracted during server-side rendering and injected into the HTML `<head>`. Without this, the page would flash unstyled content (FOUC) until the client re-generates the styles after hydration.

Wasp provides a **generic, opt-in hook** for this. Create a file at `src/ssr/styles.tsx` that exports a `createSsrStylesProvider` function:

### Emotion / MUI

```tsx title="src/ssr/styles.tsx"
import React from 'react';
import createCache from '@emotion/cache';
import { CacheProvider } from '@emotion/react';
import createEmotionServer from '@emotion/server/create-instance';

export function createSsrStylesProvider() {
  const cache = createCache({ key: 'css' });
  const { extractCriticalToChunks, constructStyleTagsFromChunks } =
    createEmotionServer(cache);

  return {
    Wrapper: ({ children }: { children: React.ReactNode }) => (
      <CacheProvider value={cache}>{children}</CacheProvider>
    ),
    extractStyles: (appHtml: string): string => {
      const chunks = extractCriticalToChunks(appHtml);
      return constructStyleTagsFromChunks(chunks);
    },
  };
}
```

Required packages: `@emotion/cache`, `@emotion/react`, `@emotion/server`

### styled-components

```tsx title="src/ssr/styles.tsx"
import React from 'react';
import { ServerStyleSheet, StyleSheetManager } from 'styled-components';

export function createSsrStylesProvider() {
  const sheet = new ServerStyleSheet();
  return {
    Wrapper: ({ children }: { children: React.ReactNode }) => (
      <StyleSheetManager sheet={sheet.instance}>{children}</StyleSheetManager>
    ),
    extractStyles: () => sheet.getStyleTags(),
  };
}
```

Required packages: `styled-components`

### Stitches

```tsx title="src/ssr/styles.tsx"
import { getCssText } from '../stitches.config';

export function createSsrStylesProvider() {
  return {
    extractStyles: () =>
      `<style id="stitches" data-stitches>${getCssText()}</style>`,
  };
}
```

### How it works

1. Wasp's SSR entry point tries to import `src/ssr/styles.tsx` at startup.
2. For each SSR request, it calls `createSsrStylesProvider()` to get a fresh provider instance (new instance per request prevents style leakage).
3. If a `Wrapper` component is returned, the React tree is wrapped with it during `renderToString`.
4. After rendering, `extractStyles(appHtml)` is called to collect the generated CSS.
5. The extracted `<style>` tags are prepended to the HTML `<head>`.

If `src/ssr/styles.tsx` does not exist, SSR proceeds without CSS-in-JS integration — no error, no crash. This is fine for projects that only use static CSS (Tailwind, plain CSS, CSS Modules).

### TypeScript types

Wasp exports the `SsrStylesProvider` interface for type safety:

```tsx title="src/ssr/styles.tsx"
import type { SsrStylesProvider } from 'wasp/client/ssr';

export function createSsrStylesProvider(): SsrStylesProvider {
  // ...
}
```

### Browser detection in SSR

Wasp automatically handles the common `typeof window !== 'undefined'` and `typeof document !== 'undefined'` browser-detection patterns. During the SSR build, these expressions are replaced with `"undefined"` so that libraries like Emotion, React, and MUI correctly take their server code paths. You don't need to configure anything — this works automatically for all packages.

:::tip
If you write custom code that needs to detect SSR, `typeof window === 'undefined'` works correctly in Wasp's SSR builds. You can also use `import.meta.env.SSR` (a Vite built-in) which is `true` in the SSR bundle and `false` in the client bundle.
:::

## Architecture (preview)

```mermaid
graph TD
  Browser -->|GET pages/assets| ClientSrv["Client/SSR server (server-ssr.mjs)"]
  ClientSrv -->|static assets| ClientBuild["build/ (client assets)"]
  ClientSrv -->|SSR render| SsrBuild["build-ssr/ (SSR bundle)"]
  Browser -->|API calls (after hydration)| ApiSrv["Wasp server (Express API)"]
  ApiSrv --> DB[(Postgres)]
```

## Limitations (current)

- No SSR in `wasp start` (dev server)
- No authenticated SSR
- No React Server Components / server actions
- No streaming / partial hydration
- No data prefetching (SSR renders with client-side data loading; API calls still happen in the browser after hydration)
- If SSR rendering fails, the server gracefully falls back to the SPA shell (client-side rendering takes over)
