{{={= =}=}}
// @ts-nocheck
import * as React from "react";
import * as ReactDOM from "react-dom/client";
import { getWaspApp } from "wasp/client/app";
{=!
  // NOTE: We are not inlining routes mapping into this file becuase once we
  // allow users to override the `index.tsx` entry point they can use the existing
  // routes mapping.
=}
{=& routesMapping.importStatement =}
{=& routeNameToSsr.importStatement =}

{=# rootComponent.isDefined =}
{=& rootComponent.importStatement =}
{=/ rootComponent.isDefined =}

{=# setupFn.isDefined =}
{=& setupFn.importStatement =}
{=/ setupFn.isDefined =}

{=# setupFn.isDefined =}
await {= setupFn.importIdentifier =}()
{=/ setupFn.isDefined =}

const app = getWaspApp({
  {=# rootComponent.isDefined =}
  rootElement: <{= rootComponent.importIdentifier =} />,
  {=/ rootComponent.isDefined =}
  routesMapping: {= routesMapping.importIdentifier =},
  routeNameToSsr: {= routeNameToSsr.importIdentifier =},
});

const rootElement = document.getElementById("root");
if (!rootElement) {
  throw new Error("Root element not found");
}

const appWithProviders = <React.StrictMode>{app}</React.StrictMode>;

if (rootElement.dataset.waspSsr === "1") {
  ReactDOM.hydrateRoot(rootElement, appWithProviders);
} else {
  ReactDOM.createRoot(rootElement).render(appWithProviders);
}
