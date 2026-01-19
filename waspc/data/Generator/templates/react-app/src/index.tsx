{{={= =}=}}
import * as React from "react";
import * as ReactDOM from "react-dom/client";
import { getWaspApp } from "wasp/client/app";
import { routesMapping } from "./router";

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
  routesMapping,
});

ReactDOM.createRoot(document.getElementById("root")!).render(
  <React.StrictMode>{app}</React.StrictMode>,
);
