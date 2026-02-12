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

## Zero-Config Activation

The entire SSR pipeline is activated automatically when at least one page has `ssr: true` in `main.wasp`. The user does not need to configure Dockerfiles, servers, package.json files, or build scripts. The chain is:

1. **Code generation** (`wasp build`): The Haskell generator checks if any page has `ssr: true`. If so, it generates `server-ssr.mjs` (the SSR HTTP server), `package.json` (with runtime dependencies like sirv), and `ssr.json` with `{ "enabled": true }`. If no page has `ssr: true`, only `ssr.json` with `{ "enabled": false }` is emitted.
2. **Build**: The deployment tooling reads `ssr.json` and, if enabled, runs an additional `vite build --ssr` to produce the `build-ssr/` bundle alongside the standard client build.
3. **Deployment**: The deployment tooling reads `ssr.json` again and selects the appropriate strategy (Node.js server for SSR vs static file server for CSR) -- see [Deployment](#deployment) below.

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

**Vite SSR config**: `ssr.noExternal: true` bundles all dependencies into the SSR output instead of leaving them as bare Node.js imports. This prevents ESM resolution errors from browser-only packages that lack proper Node.js exports (e.g., monaco-editor, react-icons).

**Prisma exclusion**: The SDK's `core/serialization/index.ts` eagerly imports `./prisma`, which registers the Prisma Decimal type with SuperJSON. This creates an import chain (`operations → serialization → prisma.ts → @prisma/client`) that would pull Prisma Client into the SSR bundle. Since the SSR server only renders HTML and never calls `serialize`/`deserialize` for operation data, the Vite plugin uses a `resolveId`/`load` hook to replace the prisma serialization module with an empty stub during SSR builds. This keeps the SSR deployment free of Prisma entirely — no `@prisma/client` dependency, no `schema.prisma`, no `prisma generate`.

### SSR / Non-SSR Page Code Splitting

**Decision**: SSR pages are statically imported, while non-SSR pages are lazy-loaded via `React.lazy` + dynamic `import()`.

**Rationale**: Non-SSR pages may depend on browser-only packages (e.g., monaco-editor). If these were statically imported, they would be pulled into the SSR bundle and crash the server at evaluation time. Lazy loading keeps their dependency trees out of the SSR bundle entirely.

- SSR pages: `import * as LandingPage from './src/pages/Landing'` (static, included in SSR bundle)
- Non-SSR pages: `const Dashboard = { default: createLazyPage(() => import('./src/pages/Dashboard')) }` (dynamic, excluded from SSR bundle)

This partitioning happens automatically in the generated `routes.tsx` virtual module based on each page's `ssr` setting.

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
  isCatchAll: boolean;
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

### Key Files Added/Modified

**New Files:**
- `waspc/data/Generator/templates/web-app/server-ssr.mjs` - SSR HTTP server (browser API polyfills, error fallback to CSR)
- `waspc/data/Generator/templates/web-app/ssr-package.json` - SSR server dependencies (sirv for static asset serving)
- `waspc/data/Generator/templates/sdk/wasp/client/vite/virtual-files/files/entry-server.tsx` - SSR entry point (route matching, rendering, head resolution, CSS-in-JS integration)
- `waspc/data/Generator/templates/sdk/wasp/client/ssr/index.ts` - `SsrStylesProvider` TypeScript interface for CSS-in-JS SSR hooks

**Modified Files (highlights):**
- `waspc/src/Wasp/AppSpec/Page.hs` - Added `ssr :: Maybe Bool` field
- `waspc/src/Wasp/AppSpec/Valid.hs` - Validation: SSR + authRequired = error
- `waspc/src/Wasp/Generator/WebAppGenerator.hs` - Conditional generation of SSR files (only when `hasSsrEnabledPage`)
- `waspc/cli/src/Wasp/Cli/Command/BuildStart/Client.hs` - SSR build step
- `waspc/data/Generator/templates/sdk/wasp/client/vite/plugins/waspConfig.ts` - Vite SSR config (`ssr.noExternal: true` to bundle all deps)
- `waspc/data/Generator/templates/sdk/wasp/client/vite/virtual-files/files/index.html` - SSR placeholders (`<!--ssr-outlet-->`, `<!--ssr-head-->`)
- `waspc/data/Generator/templates/sdk/wasp/client/vite/virtual-files/files/index.tsx` - Hydration logic (`hydrateRoot` vs `createRoot`)
- `waspc/data/Generator/templates/sdk/wasp/client/vite/virtual-files/files/routes.tsx` - Route generation with SSR/head flags
- `waspc/data/packages/deploy/src/providers/fly/commands/deploy/deploy.ts` - Fly.io SSR Dockerfile generation
- `waspc/data/packages/deploy/src/providers/railway/commands/deploy/client.ts` - Railway SSR deployment
- `waspc/data/packages/deploy/src/common/clientApp.ts` - SSR detection (`ssr.json`), SSR Vite build step

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

### Deployment

SSR changes the deployment strategy for the client app. A generated `ssr.json` config file (containing `{ "enabled": true/false }`) is used by the deployment tooling to detect whether SSR is enabled.

**Fly.io:**
- **SSR enabled**: Generates a Node.js Dockerfile that runs `server-ssr.mjs`. Includes `npm install` for the sirv static-serving dependency.
- **SSR disabled**: Uses `pierrezemb/gostatic` Docker image for static file serving (unchanged from pre-SSR behavior).

**Railway:**
- **SSR enabled**: Deploys the full `web-app/` directory. Railway's Nixpacks detects the `package.json` (with a `start` script pointing to `node server-ssr.mjs`) and runs it as a Node.js service.
- **SSR disabled**: Deploys only the `build/` subdirectory with a `Staticfile` marker for Railway's static file server (unchanged).

**Static asset serving**: The SSR server uses [sirv](https://github.com/lukeed/sirv) for serving static assets from `build/`, providing ETag support, Cache-Control headers (immutable for hashed Vite assets, 1-hour for others), and pre-compressed file serving (gzip/brotli if available).

### Browser API Polyfills

The SSR server sets up minimal browser API stubs (`window`, `document`, `localStorage`, `sessionStorage`, `HTMLElement`, etc.) before loading the SSR bundle. This prevents crashes from third-party libraries that access browser globals at module-init time.

**Import ordering**: The SSR bundle is loaded via dynamic `await import()` (not a static `import`) to ensure the polyfills are in place before any transitive dependencies are evaluated. ES module `import` statements are hoisted above all top-level code, which would otherwise cause the polyfills to run too late.

### Build-Time `typeof` Replacement

**Problem**: The runtime polyfills (e.g., `globalThis.window = globalThis`) make `typeof window !== 'undefined'` evaluate to `true` during SSR, which is incorrect. This breaks browser-detection guards in Emotion, React, MUI, socket.io, and many other libraries. For Emotion specifically, this forces the browser code path where `useInsertionEffect` (a no-op during `renderToString`) handles style injection — resulting in zero CSS in the SSR HTML.

**Solution**: The `waspConfig.ts` Vite plugin includes a `transform` hook that replaces `typeof document` and `typeof window` with `"undefined"` **only in the SSR bundle** at build time. This happens during Rollup's transform pipeline (not via Vite's `define` option, because esbuild does not support `typeof x` as a define key).

```typescript
// In the SSR bundle, after the transform:
var isBrowser = "undefined" !== "undefined"  // = false ✓
```

The replacement uses regex alternation to match string literals first (and preserve them), preventing false replacements inside strings like `"if (typeof window !== 'undefined') ..."` (which appears in papaparse and other libraries that embed code as strings).

The runtime polyfills remain as a safety net for code that accesses these globals **without** a `typeof` guard (e.g., `document.createElement()`).

**Effect**: All `typeof window` and `typeof document` browser-detection guards in the SSR bundle correctly evaluate to `false`, including:
- Emotion's `isBrowser` flag (enables server-side style injection)
- React's environment detection
- MUI's feature detection
- Wasp SDK files (`operations/internal/index.ts`, `webSocket/WebSocketProvider.tsx`)
- Any third-party library using this pattern

### CSS-in-JS SSR Support (SSR Styles Provider)

**Problem**: CSS-in-JS libraries (Emotion, styled-components, Stitches) generate styles at render time. During SSR, these styles need to be extracted and injected into the HTML `<head>` to prevent FOUC. Each library has a different API for this. Hardcoding support for any single library in Wasp would be inflexible.

**Decision**: Provide a generic, convention-based hook that users implement for their chosen CSS-in-JS library. No Haskell/AppSpec changes required.

**Convention**: If `src/ssr/styles.tsx` (or `.ts`/`.js`) exists and exports a `createSsrStylesProvider` function, Wasp's `entry-server.tsx` dynamically imports it and uses it.

```
entry-server.tsx
  ├── try { import("./src/ssr/styles") }  ← convention-based discovery
  ├── createSsrStylesProvider()           ← called once per SSR request
  │     └── returns { Wrapper?, extractStyles? }
  ├── <Wrapper>{appTree}</Wrapper>        ← wraps React tree for renderToString
  ├── renderToString(appTree)
  ├── extractStyles(appHtml)              ← collects CSS for <head>
  └── return { appHtml, headHtml: styles + headHtml }
```

**Interface** (exported from `wasp/client/ssr`):

```typescript
interface SsrStylesProvider {
  Wrapper?: React.ComponentType<{ children: React.ReactNode }>;
  extractStyles?: (appHtml: string) => string;
}
type CreateSsrStylesProvider = () => SsrStylesProvider;
```

**Rationale**:
- Package-agnostic: works with any CSS-in-JS library
- Zero-config default: projects without CSS-in-JS don't need to create the file
- Convention-based: no `main.wasp` or Haskell changes needed
- Per-request isolation: `createSsrStylesProvider()` is called for each request, preventing style leakage

**Trade-off**: The convention-based approach is less discoverable than an explicit `main.wasp` config. Mitigated by documentation and the `wasp/client/ssr` TypeScript types. A future iteration could add explicit `main.wasp` configuration (e.g., `client.ssrStylesProvider: import { ... } from "@src/ssr/styles"`) once the API stabilizes.

**Fallback behavior**: If `src/ssr/styles.tsx` doesn't exist, the `catch` block silently skips it. SSR works normally without CSS-in-JS integration. If the file exists but `createSsrStylesProvider` throws, the error is caught and logged as a warning, and SSR continues without styles.

**Interaction with the `typeof` replacement**: The build-time transform ensures Emotion's `isBrowser` flag is `false` in the SSR bundle. Combined with the SSR styles provider:
1. The `typeof` transform makes Emotion use its synchronous server code path
2. `createEmotionServer(cache)` sets `cache.compat = true`, storing CSS text in the cache
3. `extractCriticalToChunks` collects the CSS after render
4. The extracted `<style data-emotion="css ...">` tags go into the HTML `<head>`
5. On the client, Emotion's hydration picks up the existing style tags — no FOUC, no mismatch

## Future Work

- Dev mode SSR (`wasp start`) | Streaming SSR | Data prefetching | Auth-aware SSR | Response caching
- Explicit `main.wasp` configuration for SSR styles provider (e.g., `client.ssrStylesProvider: import { ... } from "@src/ssr/styles"`) as an alternative to the convention-based approach

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
