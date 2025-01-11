{{={= =}=}}
import {
  Links,
  Meta,
  Outlet,
  Scripts,
  ScrollRestoration,
} from "react-router";
import { DefaultRootErrorBoundary } from "./components/DefaultRootErrorBoundary";

export function Layout({
  children,
}: {
  children: React.ReactNode;
}) {
  return (
    <html lang="en">
      <head>
        <meta charSet="UTF-8" />
        <meta
          name="viewport"
          content="width=device-width, initial-scale=1.0"
        />
        {=& head =}

        <title>{= title =}</title>
        <Meta />
        <Links />
      </head>
      <body>
        {children}
        <ScrollRestoration />
        <Scripts />
      </body>
    </html>
  );
}


export default function Root() {
  return <Outlet />;
}

export const ErrorBoundary = DefaultRootErrorBoundary