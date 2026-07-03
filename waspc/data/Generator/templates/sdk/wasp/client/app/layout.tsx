{{={= =}=}}
import { StrictMode, type ReactNode } from "react";
import { useIsClient } from "./hooks/useIsClient.js"

export function Layout({
  children,
  isFallbackPage = false,
  clientEntrySrc,
}: {
  children?: ReactNode;
  isFallbackPage?: boolean;
  clientEntrySrc?: string;
}) {
  const isClient = useIsClient()

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

    But, if we're already in the client, we always want to render the page
    content. Whether prerendered as a fallback or not, now it's showtime, so we
    must show the user the content.

    Thus, we end up with the line below:
  */
  const shouldRenderChildren = isClient || !isFallbackPage

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
            // We pass that argument in SSR builds and not in client builds.
            // This would usually cause a hydration mismatch, but React has an
            // exception for `<script>` tags, for this specific usecase, so it
            // will work fine.
            clientEntrySrc ? (
              // The client entry is a Vite virtual module (e.g. `virtual:wasp/client-entry.tsx`).
              // A `virtual:` id is a valid ES import specifier, but not a fetchable URL, 
              // so we can't use it as a raw `<script src>`: the browser would treat `virtual:` 
              // as a dead URL scheme and never load it. Instead we load it via an inline dynamic
              // `import()`, which Vite rewrites for us: to `/@id/virtual:...` in dev and to
              // the hashed chunk in build.
              //
              // We also avoid React prerender's `bootstrapModules` option, which
              // would add a `<link rel="modulepreload">` tag that Vite doesn't
              // handle correctly.
              //
              // https://react.dev/reference/react-dom/static/prerenderToNodeStream
              <script
                type="module"
                async
                dangerouslySetInnerHTML={{
                  __html: `import(${JSON.stringify(clientEntrySrc)})`,
                }}
              />
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
