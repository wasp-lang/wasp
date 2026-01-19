export function getIndexTsxContent(): string {
  return `import * as React from "react";
  import * as ReactDOM from "react-dom/client";
  import { getWaspApp } from "wasp/client/app";
  import { routesMapping } from './routes.virtual.tsx'

  import { App as App_ext } from '../../../src/App'

  import { setup as setup_ext } from '../../../src/clientSetup'

  await setup_ext()

  const app = getWaspApp({
    rootElement: <App_ext />,
    routesMapping: routesMapping,
  });

  ReactDOM.createRoot(document.getElementById("root")!).render(
    <React.StrictMode>{app}</React.StrictMode>,
  );
`
}
