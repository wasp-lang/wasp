import React from "react";
import ReactDOM from "react-dom/client";
import { getWaspApp } from "wasp/client/app";
import { routesMapping } from "./router";

import { App } from '../../../../src/App'

import { setup } from '../../../../src/clientSetup'

await setup()

const app = getWaspApp({
  RootComponent: App,
  routesMapping,
});

ReactDOM.createRoot(document.getElementById("root") as HTMLElement).render(
  <React.StrictMode>{app}</React.StrictMode>,
);
