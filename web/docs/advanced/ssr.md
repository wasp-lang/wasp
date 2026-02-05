---
title: Server-Side Rendering (SSR)
---

SSR can be enabled per page to render HTML on the server for the initial response. This is useful for faster first paint and basic SEO use cases.

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

## Verify SSR output
- **View Page Source**: you should see HTML content inside the root element.
- **Disable JavaScript** and reload: SSR content should still render.
- The root element will include `data-wasp-ssr="1"` when SSR is used.

## How it works (high level)
- Wasp generates a small SSR server (`server-ssr.mjs`) alongside the Vite build.
- Requests to SSR‑enabled routes are rendered with `renderToString`.
- The client hydrates using `hydrateRoot` when SSR HTML is present.

## Limitations (current)
- No SSR in dev mode.
- No streaming or partial hydration.
- No automatic data prefetch/dehydrate.
- SSR is limited to public pages (no `authRequired`).

## Roadmap ideas
- Dev‑mode SSR.
- React Query prefetch + hydration.
- Auth‑aware SSR.
- Streaming SSR.
