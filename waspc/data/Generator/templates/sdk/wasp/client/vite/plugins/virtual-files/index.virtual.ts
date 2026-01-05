{{={= =}=}}
export function getIndexTsxContent(): string {
  return `import React from "react";
import ReactDOM from "react-dom/client";
import { getWaspApp } from "wasp/client/app";
{=& appComponentImport =}
{=& clientSetupImport =}
import { routes } from "{= routesVirtualFileName =}";

{=# hasClientSetup =}
setup();
{=/ hasClientSetup =}

const app = getWaspApp({
{=# hasAppComponent =}
  AppComponent: App,
{=/ hasAppComponent =}
  routes,
});

ReactDOM.createRoot(document.getElementById("root") as HTMLElement).render(
  <React.StrictMode>{app}</React.StrictMode>,
);
`;
}
