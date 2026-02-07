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

## CSS-in-JS Libraries (Emotion, styled-components, etc.)

:::caution Do you use a CSS-in-JS library?
If your project uses **Emotion** (including MUI/Material UI), **styled-components**, **Stitches**, or any other CSS-in-JS library, you **must create a configuration file** for styles to work correctly with SSR. Without it, your SSR pages will render with broken or missing styles (flash of unstyled content).

**You do NOT need this if** you only use Tailwind CSS, plain CSS, CSS Modules, or other static stylesheets — those work with SSR automatically.
:::

### Why is this needed?

CSS-in-JS libraries generate styles dynamically at render time. In a normal client-side app, this happens in the browser. But during SSR, the server renders your React components to HTML — and the generated styles must be **extracted** from that render and **injected into the HTML `<head>`** so the browser displays styled content immediately.

Wasp cannot do this automatically because each CSS-in-JS library has a different API for style extraction. Instead, Wasp provides a **hook** that you implement once for your chosen library.

### Setup (step by step)

**Step 1:** Create the file `src/ssr/styles.tsx` in your project:

```
your-project/
├── src/
│   ├── ssr/
│   │   └── styles.tsx    ← Create this file
│   ├── pages/
│   └── ...
├── main.wasp
└── ...
```

**Step 2:** Export a `createSsrStylesProvider` function from that file. Choose the example that matches your library:

#### Emotion / MUI (Material UI)

First, install the required packages (if not already installed):

```bash
npm install @emotion/cache @emotion/react @emotion/server
```

Then create the provider:

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

#### styled-components

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

#### Stitches

```tsx title="src/ssr/styles.tsx"
import { getCssText } from '../stitches.config';

export function createSsrStylesProvider() {
  return {
    extractStyles: () =>
      `<style id="stitches" data-stitches>${getCssText()}</style>`,
  };
}
```

**Step 3:** Build and test. That's it — no changes to `main.wasp` or `vite.config.ts` needed.

```bash
wasp build
wasp build start
```

View the page source in your browser. You should see `<style data-emotion="css ...">` tags (or your library's equivalent) in the `<head>`.

### How it works under the hood

Wasp's SSR entry point automatically discovers `src/ssr/styles.tsx` by convention:

1. At startup, Wasp tries to import `src/ssr/styles.tsx`. If the file doesn't exist, SSR proceeds normally without CSS-in-JS — no error.
2. For **each SSR request**, Wasp calls `createSsrStylesProvider()` to get a fresh instance. A new instance per request prevents styles from one request leaking into another.
3. If the returned object has a `Wrapper` component, Wasp wraps the React tree with it during rendering (e.g., Emotion's `CacheProvider`).
4. After `renderToString` completes, Wasp calls `extractStyles(appHtml)` to collect the CSS.
5. The extracted `<style>` tags are prepended to the HTML `<head>`, before any page-specific head tags.

### Verifying it works

After building, open an SSR page and **View Page Source** (not the DevTools Elements panel). Look for:

- **Emotion/MUI**: `<style data-emotion="css ...">` tags in the `<head>`
- **styled-components**: `<style data-styled="true">` tags in the `<head>`
- **Stitches**: `<style id="stitches">` tag in the `<head>`

If these are missing, double-check that:
1. The file is at exactly `src/ssr/styles.tsx` (not `src/styles/ssr.tsx` or any other path)
2. The function is named `createSsrStylesProvider` (or is the `default` export)
3. The required packages are installed

### TypeScript types (optional)

Wasp exports a `SsrStylesProvider` interface you can use for type safety:

```tsx title="src/ssr/styles.tsx"
import type { SsrStylesProvider } from 'wasp/client/ssr';

export function createSsrStylesProvider(): SsrStylesProvider {
  // ...
}
```

### Browser detection in SSR

Wasp automatically handles the common `typeof window !== 'undefined'` and `typeof document !== 'undefined'` browser-detection patterns used by CSS-in-JS libraries internally. During the SSR build, these expressions are replaced so that libraries correctly take their server code paths. **You don't need to configure anything** — this works automatically for all packages.

:::tip Writing SSR-safe code
If you write custom code that needs to detect whether it's running on the server or in the browser, both of these work correctly in Wasp's SSR builds:

```ts
// Option 1: standard browser detection (works thanks to Wasp's build-time transform)
if (typeof window === 'undefined') {
  // Server-side code
}

// Option 2: Vite built-in (true in SSR bundle, false in client bundle)
if (import.meta.env.SSR) {
  // Server-side code
}
```
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
