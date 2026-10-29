import { HeroUIProvider } from "@heroui/react";
import React from "react";
import ReactDOM from "react-dom/client";
import { ReactFlowProvider } from "reactflow";
import App from "./App.tsx";
import "./index.css";

ReactDOM.createRoot(document.getElementById("root")!).render(
  <React.StrictMode>
    <ReactFlowProvider>
      <HeroUIProvider>
        <div className="bg-background text-foreground dark">
          <App />
        </div>
      </HeroUIProvider>
    </ReactFlowProvider>
  </React.StrictMode>,
);
