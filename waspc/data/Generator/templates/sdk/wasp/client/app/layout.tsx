{{={= =}=}}
import { StrictMode, type ReactNode } from "react";
import { useIsClient } from "./hooks/useIsClient.js"
import { Fallback } from "./fallback.js"

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
        </head>
        <body>
          <noscript>You need to enable JavaScript to run this app.</noscript>

          {
            // We don't really need to wrap the app in a div nor name it "root",
            // but we keep it for backwards compatibility with older Wasp
            // versions.
          }
          <div id="root">
              {shouldRenderChildren ? children : <Fallback />}
          </div>

          {
            // We pass that argument in SSR builds and not in client builds.
            // This would usually cause a hydration mismatch, but React has an
            // exception for `<script>` tags, for this specific usecase, so it
            // will work fine.
            clientEntrySrc ? (
              // We'd usually use React prerender's `bootstrapModules` options for
              // injecting this script, but it would also add a `<link
              // rel="modulepreload">` tag that Vite doesn't handle correctly. So
              // we just add the script ourselves in the regular way.
              //
              // https://react.dev/reference/react-dom/static/prerenderToNodeStream
              <script
                type="module"
                src={clientEntrySrc}
                // We make it `async` to decouple the tag's position from its
                // execution phase. This way Vite can move it anywhere in the
                // document to optimize loading performance.
                async
              />
            ) : null
          }
        </body>
      </html>
    </StrictMode>
  );
}
