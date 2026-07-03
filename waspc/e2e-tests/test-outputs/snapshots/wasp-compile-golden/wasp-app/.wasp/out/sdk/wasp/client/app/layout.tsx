import { StrictMode, type ReactNode } from "react";

export function Layout({
  children,
  isFallbackPage = false,
  clientEntrySrc,
}: {
  children?: ReactNode;
  isFallbackPage?: boolean;
  clientEntrySrc?: string;
}) {
  /*
    From the Vite SSR plugin, we inherit the concept of a "prerendered page" vs.
    a "fallback page".
    - A prerendered page is a page that is rendered on the server, and then
      hydrated on the client.
    - A fallback page is a page which only prerenders the common HTML structure
      on the server, and then renders the actual page content on the client.

    To use an analogy, a fallback page is a pluripotent stem cell that can turn
    into any page in the client; while a prerendered page is already specialized
    and can only render its specific content.

    So, if we are prerendering a fallback page, we want to avoid rendering the
    actual page content, so that it can turn into anything. If we're
    prerendering a non-fallback page, we'll give it its content.

    `isFallbackPage` is only ever `true` during server prerendering of the
    fallback shell. On the client, the client entry never passes it (so it
    defaults to `false`) and instead mounts the fallback fresh via
    `createRoot(...).render()`, while hydrating prerendered pages. That means we
    always render children on the client, whether the page was prerendered as a
    fallback or not.

    Thus, we end up with the line below:
  */
  const shouldRenderChildren = !isFallbackPage

  return (
    <StrictMode>
      <html lang="en">
        <head>
          <meta charSet="utf-8" />
          <meta
            name="viewport"
            content="minimum-scale=1, initial-scale=1, width=device-width, shrink-to-fit=no"
          />

          <link rel='icon' href='/favicon.ico' />

          <title>wasp-app</title>

          {
            /*
              This script tag's job is to load the client entry so the browser
              downloads and runs it, hydrating the prerendered HTML (or, for the
              fallback shell, mounting the app fresh).

              We only need it in SSR builds, as by the time the client is
              running this code, it doesn't need to run itself again (and could
              lead to duplication).

              Rendering it only on the server and not on the client would
              normally cause a hydration mismatch, but React skips erroring on
              server-only nodes if they are **direct children** of `<head>` and
              `<body>`, to support this kind of usecase. (See
              https://react.dev/reference/react-dom/static/prerenderToNodeStream)

              The fallback shell isn't hydrated but mounted fresh with
              `createRoot`. On a fresh mount into `document`, React clears the
              singleton elements' children EXCEPT `<script>`, `<style>`,
              `<link rel="stylesheet">`, and hoistable-marked nodes, so this
              `<script>` (and the Vite-injected assets) survive. The retained
              server-shell nodes coming from the user's `head` are re-inserted
              by React, so they can appear twice in the DOM, but that's cosmetic:
              React-created `<script>` tags never execute, and duplicate
              stylesheets are harmless.

              We'd usually inject this via React prerender's `bootstrapModules`
              option, but that has two problems:
                1. React also emits a `<link rel="modulepreload"
                   href="@/wasp/client">` for the bootstrap scripts, but Vite
                   doesn't rewrite `link.href`s, so it would end up as a broken
                   link.
                2. It hardcodes `async` on the script, which in dev races the
                   `@vitejs/plugin-react` refresh preamble (see #4258).
            */
            clientEntrySrc ? (
              <script type="module" src={clientEntrySrc} />
            ) : null
          }
        </head>
        <body>
          <noscript>You need to enable JavaScript to run this app.</noscript>

          {
            // We don't really need to wrap the app in a div nor name it "root",
            // but we keep it for backwards compatibility with older Wasp
            // versions.
          }
          <div id="root">
              {shouldRenderChildren ? children : null}
          </div>
        </body>
      </html>
    </StrictMode>
  );
}
