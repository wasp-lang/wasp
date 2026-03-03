# @wasp.sh/vite-ssr

A Vite plugin that adds SSR prerendering to React Router apps. Configured routes are prerendered to static HTML at build time, while all other routes are served a client-only SPA fallback.

It is framework- and router-agnostic but designed with React 19 and React Router in mind.

## How it works

You set this plugin up in your Vite config with your **SSR entry** that `export default`s a `PrerenderFn`. This function is called for each route you want to prerender, and it returns the prerendered HTML as a Response. The plugin runs this function at build time to generate static HTML files for the specified routes.

At runtime, the plugin serves these prerendered HTML files linking to the **Client entry** for their respective routes. For any routes not prerendered, it serves a minimal SPA shell that mounts the React app client-side.

You pass it a list of **SSR paths** to prerender, and a **fallback path** used internally to generate the SPA fallback HTML. Visitors hitting an SSR path get the prerendered HTML, while all other routes serve the SPA fallback.

## Setup

### 1. Install

```bash
npm install @wasp.sh/vite-ssr
```

### 2. Configure Vite

```ts
// vite.config.ts
import react from "@vitejs/plugin-react";
import ssr from "@wasp.sh/vite-ssr";
import * as path from "node:path";
import { defineConfig } from "vite";

export default defineConfig({
  plugins: [
    react(),
    ssr({
      clientEntrySrc: path.join(import.meta.dirname, "client-entry.tsx"),
      ssrEntrySrc: path.join(import.meta.dirname, "ssr-entry.tsx"),
      ssrPaths: ["/", "/about"],
      ssrFallbackPath: "/_fallback",
    }),
  ],
});
```

**Options:**

| Option            | Description                                            |
| ----------------- | ------------------------------------------------------ |
| `clientEntrySrc`  | Absolute path to the client entry file                 |
| `ssrEntrySrc`     | Absolute path to the SSR entry file                    |
| `ssrPaths`        | Array of route paths to prerender                      |
| `ssrFallbackPath` | Path used internally to generate the SPA fallback HTML |

### 3. Write an SSR entry

The SSR entry must default-export a function matching the `PrerenderFn` type:

```ts
// ssr-entry.tsx
import type { PrerenderFn } from "@wasp.sh/vite-ssr/types";

const prerender: PrerenderFn = async (route, ctx) => {
  // Render your app to HTML using prerenderToNodeStream, renderToString, etc.
  // Use ctx.transformIndexHtml to apply Vite's HTML transforms.
  // Use ctx.clientEntrySrc to inject a <script> tag for the client bundle.
  // Return a Response with the HTML body.
};

export default prerender;
```

### 4. Write a client entry

The client entry should detect whether the page was prerendered and hydrate or mount accordingly:

```tsx
// client-entry.tsx
import { createRoot, hydrateRoot } from "react-dom/client";

// For example, React Router injects hydration data into
// window.__staticRouterHydrationData, so we can check for that:
if (window.__staticRouterHydrationData) {
  // Prerendered page — hydrate
  hydrateRoot(document, <App />);
} else {
  // SPA fallback — full client render
  createRoot(document).render(<App />);
}
```

## Requirements

- Vite 7 (uses the Environment API)
