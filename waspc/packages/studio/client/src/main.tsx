import React from "react";
import ReactDOM from "react-dom/client";
import "./index.css";
import App from "./App.tsx";
import { NextUIProvider } from "@nextui-org/react";
import { ReactFlowProvider } from "reactflow";

ReactDOM.createRoot(document.getElementById("root")!).render(
  <React.StrictMode>
    <ReactFlowProvider>
      <NextUIProvider>
        <div className="dark text-foreground bg-background">
          <App />
        </div>
      </NextUIProvider>
    </ReactFlowProvider>
  </React.StrictMode>
);
