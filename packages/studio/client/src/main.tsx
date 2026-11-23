import { ReactFlowProvider } from "@xyflow/react";
import React from "react";
import ReactDOM from "react-dom/client";
import App from "./App.tsx";
import "./index.css";

ReactDOM.createRoot(document.getElementById("root")!).render(
  <React.StrictMode>
    <ReactFlowProvider>
      <div className="bg-wasp-bg text-wasp-black font-sans">
        <App />
      </div>
    </ReactFlowProvider>
  </React.StrictMode>,
);
