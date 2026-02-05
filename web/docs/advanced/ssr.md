---
title: Server-Side Rendering (SSR)
---

SSR can be enabled per page to render HTML on the server for the initial response. This is useful for faster first paint and basic SEO use cases.
In its current form, SSR is best suited for **public, marketing/landing pages** and other content pages where an HTML first response is valuable.

## Enable SSR for a page
Add `ssr: true` to the `page` block:

```wasp
page HomePage {
  component: import { HomePage } from "@src/pages/HomePage",
  ssr: true
}
```

### Constraints
- SSR is only supported on **public pages** (pages without `authRequired`).
- Page components must be safe to render on the server (avoid direct `window`/DOM access).
- SSR does not prefetch React Query data (yet). The initial HTML is rendered without query hydration.

## Run SSR locally
SSR is currently available in the **build/preview** flow (not in `wasp start`).

From your app directory:

```bash
wasp build
```

Then run the SSR server from the generated web app:

```bash
cd .wasp/out/web-app
npm run build
npm run preview
```

Open the URL printed by `npm run preview`.

## Deploy SSR
When SSR is enabled, the web client is not a purely static build. Wasp generates:
- `server-ssr.mjs` (Node SSR server)
- `build/` (static assets)
- `build-ssr/` (SSR bundle)

In production, run the SSR server with:

```bash
node server-ssr.mjs
```

It reads the `PORT` environment variable (defaults to `4173`).

If you are using Wasp Deploy, the CLI detects SSR and deploys the web client as a Node service on Fly.io and Railway. Static hosting (Netlify/Cloudflare) works only when SSR is **not** enabled.

:::note Process management
The SSR web server is a single Node process. In production we expect it to run inside a container, and the hosting platform (Fly.io, Railway, etc.) should handle restarts and monitoring. You do not need a separate process manager like PM2.
:::

## Verify SSR output
- **View Page Source**: you should see HTML content inside the root element.
- **Disable JavaScript** and reload: SSR content should still render.
- The root element will include `data-wasp-ssr="1"` when SSR is used.

## How it works (high level)
- Wasp generates a small SSR server (`server-ssr.mjs`) alongside the Vite build.
- Requests to SSR‑enabled routes are rendered with `renderToString`.
- The client hydrates using `hydrateRoot` when SSR HTML is present.

### Impact on architecture
Previously, the web server only served static files for the SPA. With SSR enabled,
the web server becomes an **active renderer** for selected routes while still serving
static assets. The backend server remains unchanged.

Overall, the app still runs **two servers**:
- Backend server: API/auth/actions/queries.
- Web server: static assets + SSR rendering for selected routes.

## Limitations (current)
- No SSR in dev mode.
- No streaming or partial hydration.
- No automatic data prefetch/dehydrate.
- SSR is limited to public pages (no `authRequired`).
- This is classic React SSR (rendering HTML for the initial response). It does **not** include React Server Components, server actions/functions, or other “full‑stack SSR framework” features.

### What “full SSR framework” support would mean
Supporting React Server Components, server actions/functions, and streaming would require major changes to Wasp’s build pipeline, routing, runtime server, and deployment model. This is intentionally out of scope for the current, lightweight SSR layer.

## Roadmap ideas
- Dev‑mode SSR.
- React Query prefetch + hydration.
- Auth‑aware SSR.
- Streaming SSR.
