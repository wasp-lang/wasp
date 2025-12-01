import React from "react";
import ReactDOM from "react-dom/client";
import { getWaspApp } from "wasp/client/app";
import { App } from "./App";
import { setup } from "./clientSetup";
import { routes } from "./routes.generated";

setup();

const app = getWaspApp({
  AppComponent: App,
  routes,
});

ReactDOM.createRoot(document.getElementById("root") as HTMLElement).render(
  <React.StrictMode>{app}</React.StrictMode>,
);
