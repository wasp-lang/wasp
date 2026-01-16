{{={= =}=}}
export function getIndexTsxContent(): string {
  return `import React from "react";
  import ReactDOM from "react-dom/client";
  import { getWaspApp } from "wasp/client/app";
  {=& routesMapping.importStatement =}

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
    RootComponent: {= rootComponent.importIdentifier =},
    {=/ rootComponent.isDefined =}
    routesMapping: {= routesMapping.importIdentifier =},
  });

  ReactDOM.createRoot(document.getElementById("root") as HTMLElement).render(
    <React.StrictMode>{app}</React.StrictMode>,
  );
`
}
