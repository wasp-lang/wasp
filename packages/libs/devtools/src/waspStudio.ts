import type { DevToolsChildProcessTerminalSession } from "@vitejs/devtools-kit";
import { createProcessLauncher } from "@vitejs/devtools-kit/node";
import type { Plugin } from "vite";

import { findFreePort } from "./findFreePort";
import { waitForServer } from "./waitForServer";

const WASP_STUDIO_ID = "wasp-studio";

/**
 * Adds Wasp Studio as a panel to Vite DevTools. The panel runs `wasp studio`
 * on a free port when the user opens it, and embeds it once it's up.
 */
export function waspStudio(): Plugin {
  let projectRoot = process.cwd();
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
          // `wasp start` passes the path to its own executable, so the panel
          // uses the same Wasp version as the app.
          command: process.env["WASP_EXE_PATH"] ?? "wasp",
          args: ["studio", "--port", String(port)],
          cwd: projectRoot,
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
    configResolved(config) {
      projectRoot = config.root;
    },
    configureServer(server) {
      // Vite DevTools leaves the process running when the dev server stops.
      // `terminate` also stops the process's children, like the server that
      // `wasp studio` starts.
      server.httpServer?.once("close", () => {
        void studioSession?.terminate();
      });
    },
  };
}
