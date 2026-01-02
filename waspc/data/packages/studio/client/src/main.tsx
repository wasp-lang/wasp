import { NextUIProvider } from "@nextui-org/react";
import React from "react";
import ReactDOM from "react-dom/client";
import { ReactFlowProvider } from "reactflow";
import App from "./App.tsx";
import "./index.css";

ReactDOM.createRoot(document.getElementById("root")!).render(
  <React.StrictMode>
    <ReactFlowProvider>
      <NextUIProvider>
        <div className="bg-background text-foreground dark">
          <App />
        </div>
      </NextUIProvider>
    </ReactFlowProvider>
  </React.StrictMode>,
);
