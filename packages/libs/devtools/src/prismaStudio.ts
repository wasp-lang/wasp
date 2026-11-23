import type { DevToolsChildProcessTerminalSession } from "@vitejs/devtools-kit";
import { createProcessLauncher } from "@vitejs/devtools-kit/node";
import type { Plugin } from "vite";

import { findFreePort } from "./findFreePort";
import { waitForServer } from "./waitForServer";

const PRISMA_STUDIO_ID = "wasp-prisma-studio";

/**
 * Adds Prisma Studio as a panel to Vite DevTools. The panel runs
 * `wasp db studio` on a free port when the user opens it, and embeds it once
 * it's up.
 */
export function prismaStudio(): Plugin {
  let projectRoot = process.cwd();
  let studioUrl = "";
  let studioSession: DevToolsChildProcessTerminalSession | undefined;

  return {
    ...createProcessLauncher({
      id: PRISMA_STUDIO_ID,
      title: "Prisma Studio",
      icon: "simple-icons:prisma",
      description: "Browse and edit the data in your database.",
      buttonStart: "Start Prisma Studio",
      buttonLoading: "Starting Prisma Studio…",
      process: async () => {
        const port = await findFreePort();
        studioUrl = `http://localhost:${port}/`;
        return {
          // `wasp start` passes the path to its own executable, so the panel
          // uses the same Wasp version as the app.
          command: process.env["WASP_EXE_PATH"] ?? "wasp",
          args: ["db", "studio", "--port", String(port)],
          cwd: projectRoot,
          // Prisma Studio would otherwise open its own browser tab.
          env: { BROWSER: "none" },
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
      // `terminate` also stops the process's children, like the Prisma Studio
      // that `wasp db studio` starts.
      server.httpServer?.once("close", () => {
        void studioSession?.terminate();
      });
    },
  };
}
