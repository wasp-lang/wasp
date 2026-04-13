---
title: Prerendering
---

By default, Wasp apps are single-page applications: the browser downloads JavaScript, and React renders the page on the client. This means search engines, AI crawlers, and users on slow connections see a blank page until JavaScript loads and executes.

Wasp can **prerender** specific routes at build time, producing static HTML files that are served immediately. The page then hydrates on the client for full interactivity.

This gives you:

- **Better SEO:** search engines index real HTML content instead of an empty shell.
- **LLM and AI readability:** AI crawlers (ChatGPT, Perplexity, Claude, etc.) can read your content directly.
- **Faster performance on user experience:** users see content immediately (better [Largest Contentful Paint](https://developer.chrome.com/docs/lighthouse/performance/lighthouse-largest-contentful-paint)), with no layout shift from content loading in (better [Cumulative Layout Shift](https://web.dev/articles/cls)).
- **Works without JavaScript:** content is visible even before the browser loads your JS bundle.

## Enabling prerendering

Add `prerender: true` to any route declaration:

```wasp title="main.wasp"
route LandingRoute { path: "/", to: LandingPage, prerender: true }
page LandingPage {
  component: import { LandingPage } from "@src/LandingPage"
}
```

That's it. When you run `wasp build`, Wasp renders this route's HTML at build time. The generated HTML is served directly to browsers and crawlers, then we [hydrate](https://react.dev/reference/react-dom/client/hydrateRoot) the page for full interactivity.

## How it works

By default, `wasp build` generates a single `200.html` file that serves as the entry point for all routes. When a request comes in, the server sends this HTML, and React renders the appropriate page on the client. This is called a Single-Page Application (SPA) architecture.

But for `prerender: true` routes, Wasp will call them at build time, and render your page components as HTML, with special markers to allow for hydration. This HTML is then written to a file placed in the build output alongside the SPA file.

When a request hits a prerendered route's path, the server sends the pre-built HTML directly. Once the browser loads the JavaScript bundle, React hydrates the static HTML into a fully interactive app, no second render needed.

Routes without `prerender: true` continue to work as before: the server sends the SPA file, and the client renders the page from scratch.

## When to use prerendering

Prerendering works best for pages where the content is known at build time:

- Landing pages and marketing pages
- About, pricing, and FAQ pages
- Blog posts or documentation
- Any page with mostly static content that doesn't depend on the logged-in user

:::tip
Prerendering is especially valuable if you want your content to be indexed by search engines or readable by AI assistants like ChatGPT, Perplexity, or Claude.
:::

## Limitations

### Static paths only

Prerendering only works on routes with static paths. Routes with dynamic segments (`:paramName`), optional segments (`?`), or splats (`*`) cannot be prerendered, because the HTML must be generated at build time for a known URL.

```wasp title="main.wasp"
// ✅ Works (static path)
route AboutRoute { path: "/about", to: AboutPage, prerender: true }

// ❌ Won't compile (dynamic segment)
route UserRoute { path: "/user/:id", to: UserPage, prerender: true }
```

### No auth-required pages

Routes pointing to pages with `authRequired: true` cannot be prerendered, since the page content depends on the logged-in user.

```wasp title="main.wasp"
// ❌ Won't compile (authRequired is true)
route DashRoute { path: "/dashboard", to: DashPage, prerender: true }
page DashPage {
  authRequired: true,
  component: import { DashPage } from "@src/DashPage"
}
```

:::caution
Wasp reports an error at compile time if you try to prerender a route with a dynamic path or an auth-required page.
:::

## Troubleshooting

### Hydration mismatches

When React hydrates a prerendered page, it expects the prerendered HTML to match what the client renders. If they differ, React logs a warning and may discard the prerendered HTML, losing the performance benefits.

#### Common causes

- **Checking for `window` or `document`:** code like `typeof window !== 'undefined'` or `import.meta.env.SSR` returns different values on the prerender vs. the client, and might change everything that depends on it.
- **Non-deterministic values during render:** functions like `Date.now()`, or `Math.random()` produces different results on each render.
- **Browser-only APIs:** accessing `window.innerWidth`, `navigator.userAgent`, `localStorage`, or similar APIs during render will fail while prerendering. This also applies to some third-party libraries that access these APIs, or less obvious JS APIs like `Intl.DateTimeFormat` that can use different timezones and locales on the prerender vs. client.

#### How to fix: the `useIsClient` pattern

The fix is to render the same content on both the prerender and the client during the initial render, then add client-only behavior after hydration using `useEffect`.

Here's an example of the **wrong** approach:

```tsx title="src/LandingPage.tsx" auto-js
// ❌ Causes a hydration mismatch
export function LandingPage() {
  const isClient = typeof window !== 'undefined'
  return <p>{isClient ? 'Client content' : 'Prerendered content'}</p>
}
```

And the **correct** approach:

```tsx title="src/LandingPage.tsx" auto-js
// ✅ No hydration mismatch
import { useState, useEffect } from 'react'

function useIsClient() {
  const [isClient, setIsClient] = useState(false)
  useEffect(() => {
    setIsClient(true)
  }, [])
  return isClient
}

export function LandingPage() {
  const isClient = useIsClient()
  return <p>{isClient ? 'Client content' : 'Prerendered content'}</p>
}
```

## API reference

### `prerender` field on `route`

```wasp title="main.wasp"
route NameRoute {
  path: "/some-path",
  to: SomePage,
  prerender: true,   // optional, defaults to false
}
```

`prerender` is an optional boolean field on the `route` declaration.

When set to `true`, Wasp prerenders the route's page component at build time, producing a static HTML file for that path.

**Requirements:**
- The route path must be fully static (no `:paramName`, `*`, or `?` segments).
- The target page must not have `authRequired: true`.
