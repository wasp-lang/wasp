export function getIndexTsxContent(): string {
  return `import React from "react";
  import ReactDOM from "react-dom/client";
  import { getWaspApp } from "wasp/client/app";
  import { routesMapping } from './routes.virtual.tsx'




  const app = getWaspApp({
    routesMapping,
  });

  ReactDOM.createRoot(document.getElementById("root") as HTMLElement).render(
    <React.StrictMode>{app}</React.StrictMode>,
  );
`
}
