# @wasp.sh/vite-ssr

A Vite plugin that adds SSR prerendering to web apps. Configured routes are prerendered to static HTML at build time, while all other routes are served a client-only SPA fallback.

It is framework- and router-agnostic but designed with our internal usage of React 19 and React Router in mind.

## Overview

You set this plugin up in your Vite config with your **SSR entry** that `export default`s a `PrerenderFn`. This function is called for each route you want to prerender, and it returns the prerendered HTML. The plugin runs this function at build time to generate static HTML files for the specified routes.

At runtime, the plugin serves these prerendered HTML files linking to the **Client entry** for their respective routes. For any routes not prerendered, it serves a minimal SPA shell that mounts the React app client-side.

You pass it a list of **SSR paths** to prerender, and a **fallback path** used internally to generate the SPA fallback HTML. Visitors hitting an SSR path get the prerendered HTML, while all other routes serve the SPA fallback.

See a more in-depth explanation of the plugin's architecture and how it works [in the FAQ section](#faq) of this README.

## Example setup

This section assumes a React 19 app with React Router 7, but the plugin itself is framework- and router-agnostic. You can adapt it for your own setup as needed.

### 1. Install

Note: this plugin is not published to npm but we're writing it as if it could be in the future. For now, it isn't available on npm.

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
      ssrFallbackFile: "/_fallback.html",
    }),
  ],
});
```

**Options:**

| Option            | Description                               |
| ----------------- | ----------------------------------------- |
| `clientEntrySrc`  | Absolute path to the client entry file    |
| `ssrEntrySrc`     | Absolute path to the SSR entry file       |
| `ssrPaths`        | Array of route paths to prerender         |
| `ssrFallbackFile` | Where the fallback HTML will be generated |

### 3. Write an SSR entry

The SSR entry must default-export a function matching the `PrerenderFn` type:

```tsx
// ssr-entry.tsx
import type { PrerenderFn } from "@wasp.sh/vite-ssr/types";
import { prerenderToNodeStream } from "react-dom/static";
import App from "./app";
import * as streamConsumers from "node:stream/consumers";

const prerender: PrerenderFn = async (route, ctx) => {
  const isFallback = route === "/_fallback.html";

  const html = await streamConsumers.text(
    prerenderToNodeStream(
      <App
        scriptSrc={ctx.clientEntrySrc}
        renderType={
          isFallback ? { type: "fallback" } : { type: "route", route }
        }
      />,
    ),
  );

  return html;
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

## FAQ

### Prerendered vs. fallback page

#### Explanation

When you start prerendering pages in an app, you implicitly create two kinds of HTML files that the server sends to the client.

Let's do an analogy. In your body, all the cells have the same DNA, that gets expressed differently. When you need a new red blood cell, you might have an already specialized cell available in your body's storage; or otherwise you might have to send a stem cell that has the ability to turn into anything. But regardless, when either cell reaches the bloodstream, they both have to be able to act like a red blood cell!

In the same way, with prerendering, we can create different kinds of outputs from the same source code. When a page is requested, you might have done the work ahead of time and send a prepopulated HTML. Or you might send a blank HTML that has just enough code so that it can turn into any page of the app. Regardless, whenever either of the HTML pages reach the client, it's showtime, and they must "turn on" and start presenting the page the user requested.

#### Application

The Vite SSR plugin can output two different kinds of pages:

- A **prerendered** page that is specialized to the requested route, and then hydrated in the client
- A **fallback** page that is mostly blank with just the "shell", and then fully rendered on the client

Let's see how this works for an app that has only one prerendered route (`/about`):

- At build time, we tell the app to pre-render the route `/about`. We store the result of that as `about.html`. This file has the SSR'd components, and a special "hydration data" that tells the router which route has been prerendered, and any internal bookkeeping (in React Router's case, it also stores the results of the route's `loaders`, but we don't use that feature):

  ```html
  <html>
    <head>
      <title>About | My Corporation LLC</title>
      <link rel="favicon" href="/favicon.ico">
    </head>
    <body>
      <div id="app">
        <h1>About My Corporation</h1>
        <p>Lorem ipsum dolor sit amet</p>
      </div>
      <script>
        window.__hydration_data__ = { route: "/about" };
      </script>
      <script src="/app.js"></script>
    </body>
  </html>
  ```

- At build time, we also tell the app to give us a blank page that can turn into anything. We store that as `_fallback.html`. This file just has the most common parts of the app:

  ```html
  <html>
    <head>
      <title>Loading... | My Corporation LLC</title>
      <link rel="favicon" href="/favicon.ico">
    </head>
    <body>
      <div id="app">
        <p>Loading...</p>
      </div>
      <script src="/app.js"></script>
    </body>
  </html>
  ```

- Then, in the server, when we receive a request for `/about`, we can just find the `about.html` file and send it directly to the client. The `app.js` script will hydrate it.
- But if we have to serve e.g. `/faq`, we send the `_fallback.html` file, which is just blank, and in the client it will get the correct bundle and render it, we'll just have to wait a bit until it does.

In both cases, the `app.js` content is the same:

```tsx
import { lazy, Suspense } from "react";

const routes = {
  "/about": { Component: lazy(async () => import("/chunks/about-page.js")) },
  "/faq": { Component: lazy(async () => import("/chunks/faq-page.js")) },
};

const Router = ({ hydrationData }) => {
  const routeToRender = hydrationData?.route ?? window.location.pathname;
  const routeData = routes[routeToRender];
  return (
    <Suspense fallback={<p>Loading...</p>}>
      <routeData.Component />
    </Suspense>
  );
};

hydrate(<Router routes={routes} hydrationData={window.__hydration_data__} />);
```

### Vite plugin vs. application entries

The Vite plugin is structured such that it is framework- and router-agnostic. While we use it for React Router, it doesn't need to know anything about it, or indeed about React itself.

The main insight comes from seeing that, in the prerendering world, your app really comes down to just two functions:

- The **ssrEntry:** a function that runs at build-time and converts predefined routes into `.html` files. e.g. `prerenderApp("/about")` should give me `<html><body><h1>About My Company</h1><button>Contact us</button>`.
- The **clientEntry:** a function that runs in the browser and takes the HTML and turns it into an interactive thing. e.g. `hydrateApp()` will inspect the current HTML and hook up a modal on the "Contact us" button.

We don't need anything else. This gives us a clear separation of concerns where the central part is this flexible but well-scoped API. The Vite plugin takes charge of hooking into Vite in the right way, so that it knows when to call these functions and with which parameters. Your app takes charge of providing the entries that make sense to your framework and your router, and do any setup they need to.

The flexibility and small size of this API also allows you to build additional layers on top that are more expressive or conventional, so it's great for using it as the SSR provider for a framework. Like we're doing for Wasp.
