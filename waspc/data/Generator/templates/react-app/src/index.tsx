{{={= =}=}}
import React from "react";
import ReactDOM from "react-dom/client";
import { getWaspApp } from "wasp/client/app";
import { routesMapping } from "./router";

{=# rootComponent.isDefined =}
{=& rootComponent.importStatement =}
{=/ rootComponent.isDefined =}

{=# setupFn.isDefined =}
{=& setupFn.importStatement =}
{=/ setupFn.isDefined =}

// TODO: see if this needs to be passed into the `getWaspApp` because of the `await`
{=# setupFn.isDefined =}
await {= setupFn.importIdentifier =}()
{=/ setupFn.isDefined =}

const app = getWaspApp({
  {=# rootComponent.isDefined =}
  AppComponent: {= rootComponent.importIdentifier =},
  {=/ rootComponent.isDefined =}
  routesMapping,
});

ReactDOM.createRoot(document.getElementById("root") as HTMLElement).render(
  <React.StrictMode>{app}</React.StrictMode>,
);
