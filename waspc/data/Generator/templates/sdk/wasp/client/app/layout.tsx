{{={= =}=}}
import { Suspense } from "@suspensive/react";
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
            // We don't really need to wrap the app a div nor name it "root",
            // but we keep it for backwards compatibility with older Wasp
            // versions.
          }
          <div id="root">
            <Suspense
              clientOnly={isFallbackPage}
              fallback={<p>Loading...</p>}
            >
              {children}
            </Suspense>
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
