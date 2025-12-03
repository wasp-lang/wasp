import React from "react";
import ReactDOM from "react-dom/client";
import { getWaspApp } from "wasp/client/app";
import { routes } from "./routes.generated";

const app = getWaspApp({ routes });

ReactDOM.createRoot(document.getElementById("root") as HTMLElement).render(
  <React.StrictMode>{app}</React.StrictMode>,
);
