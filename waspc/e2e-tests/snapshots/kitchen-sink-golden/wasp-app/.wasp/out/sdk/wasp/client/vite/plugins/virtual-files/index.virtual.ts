export function getIndexTsxContent(): string {
  return `import React from "react";
  import ReactDOM from "react-dom/client";
  import { getWaspApp } from "wasp/client/app";
  import { routesMapping } from './routes.virtual.tsx'

  import { App as App_ext } from './src/App'

  import { setup as setup_ext } from './src/clientSetup'

  await setup_ext()

  const app = getWaspApp({
    RootComponent: App_ext,
    routesMapping: routesMapping,
  });

  ReactDOM.createRoot(document.getElementById("root") as HTMLElement).render(
    <React.StrictMode>{app}</React.StrictMode>,
  );
`
}
