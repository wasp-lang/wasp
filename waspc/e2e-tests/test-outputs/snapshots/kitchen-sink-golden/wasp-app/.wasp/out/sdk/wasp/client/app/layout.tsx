import { StrictMode, type ReactNode, useSyncExternalStore } from "react";

export function Layout({
  children,
  isFallbackPage = false,
  clientEntrySrc,
}: {
  children?: ReactNode;
  isFallbackPage?: boolean;
  clientEntrySrc?: string;
}) {
  const shouldRenderAppContent = useShouldRenderAppContent(isFallbackPage);

  return (
    <StrictMode>
      <html lang="en">
        <head>
          <meta charSet="utf-8" />
          <meta
            name="viewport"
            content="minimum-scale=1, initial-scale=1, width=device-width, shrink-to-fit=no"
          />

          <link rel={"manifest"} href={"/manifest.json"} />
<link rel={"icon"} href={"/favicon.ico"} />

          <title>Wasp Kitchen Sink</title>

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
          {shouldRenderAppContent ? children : null}
        </body>
      </html>
    </StrictMode>
  );
}

function useShouldRenderAppContent(isFallbackPage: boolean) {
  // We always want to render the content on the client.
  const getOnClient = () => true;

  // On the server, we only want to render the content if it's not a fallback page.
  const getOnServer = () => !isFallbackPage;

  const shouldRenderAppContent =
    // We use `useSyncExternalStore` because it allows us to have different
    // values on the server and client without hydration errors. The semantics
    // also match, as in this case the fallback status is the synchronous state
    // we are reading from, it just never changes after being first initialized.
    useSyncExternalStore(
      emptySubscribe,
      getOnClient,
      getOnServer,
    );

  return shouldRenderAppContent;
}

// The subscribe function is expected to only change when the store changes.
// Because our "store" is static, we make our function never change by putting
// it on the top.
function emptySubscribe() {
  const emptyUnsubscribe = () => {};
  return emptyUnsubscribe;
}
