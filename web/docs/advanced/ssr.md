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
