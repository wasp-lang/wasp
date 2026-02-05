# Server-Side Rendering (SSR) Support

This document describes the design for adding Server-Side Rendering (SSR) support to Wasp on a per-page basis.

## Motivation

Server-Side Rendering improves:
- **SEO**: Search engines can index fully rendered HTML content
- **Initial Load Performance**: Users see content faster (especially on slow connections)
- **Social Sharing**: Open Graph meta tags are rendered server-side for link previews

Currently, Wasp apps are purely client-side rendered (CSR), which means the initial HTML is an empty shell that gets populated by JavaScript after load.

## Requirements

### Basic
- Pages can opt-in to SSR via a `ssr: true` property
- SSR pages must be public (no `authRequired: true`) - this simplifies the initial implementation
- Non-SSR pages continue to work as before (CSR)
- Smart hydration: SSR pages use `hydrateRoot`, CSR pages use `createRoot`
- Support for a lightweight per-page `head` export (title/meta/link) for SSR

### Non-Goals (this iteration)
- SSR in dev mode (`wasp start`) | Streaming/partial hydration | React Query data prefetch
- Auth-aware SSR (public pages only) | React Server Components | Server actions/functions

### Scope
This is a lightweight SSR for public/marketing pages where HTML-first matters. Not a full SSR framework.

> **Why not full SSR?** RSC, server actions, and streaming would require: split server/client component graphs, Flight manifest, streaming runtime, new routing model, server action transport/security, and turning the web client into a server runtime. Out of scope for this POC.

## Design Decisions

### Per-Page SSR Opt-In

**Decision**: SSR is enabled per-page via `ssr: true` in the page declaration.

```wasp
page LandingPage {
  component: import { Landing } from "@src/pages/Landing",
  ssr: true
}
```

**Rationale**: 
- Not all pages benefit from SSR (e.g., dashboards behind auth)
- Keeps the default behavior (CSR) unchanged
- Gives developers fine-grained control

**Trade-off**: More configuration vs. app-wide SSR flag. Per-page is more flexible.

### SSR Only for Public Pages

**Decision**: Pages with `authRequired: true` cannot have `ssr: true`.

**Rationale**:
- SSR with authentication requires handling cookies/sessions on the server
- The SSR server would need access to the auth system
- Adds significant complexity for limited benefit (auth pages are typically not SEO-critical)

**Trade-off**: Limits SSR to public pages. Future work could add authenticated SSR.

### Separate SSR Server

**Decision**: A dedicated Node.js HTTP server (`server-ssr.mjs`) serves the production build with SSR support.

**Rationale**:
- Keeps SSR concerns separate from the main Wasp server
- The SSR server only needs to import the client build, not the full server
- Simpler deployment story - can be deployed alongside or separately

**Alternative Considered**: Integrating SSR into the main Wasp server. Rejected because:
- Would couple client rendering to server deployment
- More complex server setup
- Harder to scale independently

### Build Pipeline

**Decision**: Two Vite builds for SSR-enabled apps:
1. Standard client build: `vite build` → `web-app/build/`
2. SSR build: `vite build --ssr` → `web-app/build-ssr/`

**Rationale**: Vite's recommended approach for SSR. The SSR build produces a Node.js-compatible bundle.

### Hydration Detection

**Decision**: Use a `data-wasp-ssr="1"` attribute on the root element to detect SSR.

```html
<div id="root" data-wasp-ssr="1"><!-- SSR content --></div>
```

Client code checks this attribute:
```tsx
if (rootElement.dataset.waspSsr === "1") {
  ReactDOM.hydrateRoot(rootElement, app);
} else {
  ReactDOM.createRoot(rootElement).render(app);
}
```

**Rationale**: Simple, reliable, no additional network requests or global variables.

### Route Matching on Server

**Decision**: The SSR server uses `react-router`'s `matchRoutes` to determine if a URL matches an SSR-enabled route.

```typescript
export function getRouteMatchInfo(url: string): {
  matched: boolean;
  ssr: boolean;
  outsideBase: boolean;
}
```

**Rationale**: Reuses the same routing logic as the client, ensuring consistency.

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Production Build                          │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  ┌──────────────┐    ┌──────────────┐    ┌──────────────┐  │
│  │ index.html   │    │ build/       │    │ build-ssr/   │  │
│  │ (template)   │    │ (client JS)  │    │ (SSR bundle) │  │
│  └──────────────┘    └──────────────┘    └──────────────┘  │
│         │                   │                   │           │
│         └───────────────────┼───────────────────┘           │
│                             │                               │
│                    ┌────────▼────────┐                      │
│                    │  server-ssr.mjs │                      │
│                    │  (HTTP Server)  │                      │
│                    └────────┬────────┘                      │
│                             │                               │
└─────────────────────────────┼───────────────────────────────┘
                              │
              ┌───────────────┼───────────────┐
              │               │               │
        ┌─────▼─────┐   ┌─────▼─────┐   ┌─────▼─────┐
        │  Static   │   │  SSR      │   │  CSR      │
        │  Assets   │   │  Routes   │   │  Routes   │
        │  (pass)   │   │  (render) │   │  (shell)  │
        └───────────┘   └───────────┘   └───────────┘
```

### Request Flow

1. Request arrives at SSR server
2. If static asset → serve from `build/` directory
3. If route matches and has `ssr: true`:
   - Render with `renderToString`
   - Inject HTML into `<!--ssr-outlet-->`
   - Inject head tags into `<!--ssr-head-->`
   - Add `data-wasp-ssr="1"` to root div
4. Otherwise → serve shell `index.html` for client-side routing

## Implementation Details

### API Calls Are Still Client-Side

This iteration keeps Wasp's existing client/server split:

- The **client/SSR server** (`server-ssr.mjs`) is responsible for serving static assets and (optionally) rendering HTML for `ssr: true` routes.
- The **Wasp server** (Express) continues to serve the API.

Importantly, the SSR server **does not call the Wasp API** during rendering (no request header/cookie forwarding, no data prefetching). Any API calls still happen from the **browser after hydration**.

This keeps the feature focused on landing/marketing pages and avoids the larger design surface of authenticated SSR and data fetching.

### Files Added/Modified

**New Files:**
- `waspc/data/Generator/templates/web-app/server-ssr.mjs` - SSR HTTP server
- `waspc/data/Generator/templates/sdk/wasp/client/vite/virtual-files/files/entry-server.tsx` - SSR entry point

**Modified Files:**
- `waspc/src/Wasp/AppSpec/Page.hs` - Added `ssr :: Maybe Bool` field
- `waspc/src/Wasp/AppSpec/Valid.hs` - Validation: SSR + authRequired = error
- `waspc/cli/src/Wasp/Cli/Command/BuildStart/Client.hs` - SSR build step
- `waspc/data/Generator/templates/sdk/wasp/client/vite/virtual-files/files/index.html` - SSR placeholders
- `waspc/data/Generator/templates/sdk/wasp/client/vite/virtual-files/files/index.tsx` - Hydration logic

### Head Management (Lightweight)

**Decision**: Allow each page component module to optionally export a `head` function.

```ts
export const head = () => ({
  title: "Home",
  meta: [
    { name: "description", content: "Landing page" },
    { property: "og:title", content: "Home" }
  ],
  link: [
    { rel: "canonical", href: "https://example.com" }
  ]
});
```

The SSR entry reads this export and injects the resulting tags into `<!--ssr-head-->`.

**Rationale**:
- No new dependencies
- No config changes
- Good enough for landing pages and basic SEO

**Trade-off**: Not a full head management solution (no dynamic/async data or client updates).

**Title Override**: If a page defines a `title` in its `head` export, it overrides the global `app.title`. The server removes the global `<title>` tag before injecting the page-level head.

### 404 Pages (Catch-All Routes)

**Decision**: Catch-all routes (`path: "*"`) automatically receive HTTP 404 status, regardless of SSR setting.

```wasp
route CatchAllRoute { path: "*", to: NotFoundPage }
page NotFoundPage {
  component: import { NotFoundPage } from "@src/pages/NotFoundPage",
  ssr: true  // Recommended for SEO and accessibility
}
```

**Rationale**:
- Proper HTTP status codes are important for SEO (search engines should know it's a 404)
- The catch-all route semantically represents "page not found"

**With `ssr: true`**: 404 status + server-rendered content (visible without JS)
**Without `ssr: true`**: 404 status + empty shell (content requires JS)

**Recommendation**: Add `ssr: true` to catch-all pages so the 404 content is visible to crawlers and users with JavaScript disabled.

## Future Work

- Dev mode SSR (`wasp start`) | Streaming SSR | Data prefetching | Auth-aware SSR | Response caching | CSR fallback on error

## Usage Example

```wasp
app MyApp {
  title: "My App"
}

route LandingRoute { path: "/", to: LandingPage }
page LandingPage {
  component: import { Landing } from "@src/pages/Landing",
  ssr: true  // This page will be server-side rendered
}

route DashboardRoute { path: "/dashboard", to: DashboardPage }
page DashboardPage {
  component: import { Dashboard } from "@src/pages/Dashboard",
  authRequired: true  // This page uses CSR (cannot combine with ssr: true)
}
```

Build and run:
```bash
wasp build
wasp build start
```

## Local Development (for Wasp developers)

When working on the SSR feature from within the Wasp repo (e.g., `examples/kitchen-sink`), use the following workflow:

```bash
# Terminal 1: Start the managed database
../../waspc/run wasp-cli db start

# Note the DATABASE_URL printed in the output, e.g.:
# postgresql://postgresWaspDevUser:postgresWaspDevPass@localhost:5432/KitchenSink-xxxxx

# Terminal 2: Generate the code
../../waspc/run wasp-cli build

# Terminal 3: Build and run with the database credentials
../../waspc/run wasp-cli build start \
  --server-env DATABASE_URL=postgresql://postgresWaspDevUser:postgresWaspDevPass@localhost:5432/KitchenSink-xxxxx \
  --server-env JWT_SECRET=dev-secret-change-me
```

Open:
- `http://localhost:3000` - SSR client
- `http://localhost:3001` - API server

The SSR server will:
- Render `/` server-side (Landing page)
- Serve `/dashboard` as client-side rendered (Dashboard page)
