{{={= =}=}}
import { StrictMode, type ReactNode } from "react";

export function Layout({
  children,
  clientEntrySrc,
}: {
  children?: ReactNode;
  clientEntrySrc?: string;
}) {
  return (
    <StrictMode>
      <html lang="en">
        <head>
          <meta charSet="utf-8" />
          <meta
            name="viewport"
            content="minimum-scale=1, initial-scale=1, width=device-width, shrink-to-fit=no"
          />

          {=& head =}

          <title>{= title =}</title>

          {
            /*
              This script tag's job is to load the client entry so the browser
              downloads and runs it, hydrating the prerendered HTML.

              We only need it in SSR builds, as by the time the client is
              running this code, it doesn't need to run itself again (and could
              lead to duplication).

              Rendering it only on the server and not on the client would
              normally cause a hydration mismatch, but React skips erroring on
              server-only nodes if they are **direct children** of `<head>` and
              `<body>`, to support this kind of usecase. (See
              https://react.dev/reference/react-dom/static/prerenderToNodeStream)

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
          {children}
        </body>
      </html>
    </StrictMode>
  );
}
