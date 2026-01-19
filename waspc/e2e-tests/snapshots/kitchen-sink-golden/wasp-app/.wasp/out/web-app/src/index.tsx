import * as React from "react";
import * as ReactDOM from "react-dom/client";
import { getWaspApp } from "wasp/client/app";
import { routesMapping } from "./router";

import { App } from '../../../../src/App'

import { setup } from '../../../../src/clientSetup'

await setup()

const app = getWaspApp({
  rootElement: <App />,
  routesMapping,
});

ReactDOM.createRoot(document.getElementById("root")!).render(
  <React.StrictMode>{app}</React.StrictMode>,
);
