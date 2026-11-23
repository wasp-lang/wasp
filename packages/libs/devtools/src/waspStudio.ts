import type { DevToolsChildProcessTerminalSession } from "@vitejs/devtools-kit";
import { createProcessLauncher } from "@vitejs/devtools-kit/node";
import type { Plugin } from "vite";
import { findFreePort, waitForServer, WASP_COMMAND_BIN } from "./common";

const WASP_STUDIO_ID = "wasp-studio";

export function waspStudio(): Plugin {
  let studioUrl = "";
  let studioSession: DevToolsChildProcessTerminalSession | undefined;

  return {
    ...createProcessLauncher({
      id: WASP_STUDIO_ID,
      title: "Wasp Studio",
      icon: "ph:graph-duotone",
      description: "See how the parts of your app connect.",
      buttonStart: "Start Wasp Studio",
      buttonLoading: "Starting Wasp Studio…",
      process: async () => {
        const port = await findFreePort();
        studioUrl = `http://localhost:${port}/`;
        return {
          command: WASP_COMMAND_BIN,
          args: ["studio", "--port", String(port)],
        };
      },
      serve: {
        onReady: async (session) => {
          studioSession = session;
          await waitForServer(studioUrl, () => session.status === "running");
          return studioUrl;
        },
      },
    }),
    apply: "serve",
    configureServer(server) {
      server.httpServer?.once("close", () => {
        void studioSession?.terminate();
      });
    },
  };
}
