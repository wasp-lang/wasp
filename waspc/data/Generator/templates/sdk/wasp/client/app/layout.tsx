{{={= =}=}}
import { Suspense } from "@suspensive/react";
import { StrictMode, type ReactNode } from "react";

export function Layout({
  children,
  isFallbackPage = false,
}: {
  children?: ReactNode;
  isFallbackPage?: boolean;
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
          <div id="root">
            <Suspense
              clientOnly={isFallbackPage}
              fallback={<p>Loading...</p>}
            >
              {children}
            </Suspense>
          </div>
        </body>
      </html>
    </StrictMode>
  );
}
