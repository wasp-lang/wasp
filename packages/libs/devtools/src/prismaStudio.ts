import type { DevToolsChildProcessTerminalSession } from "@vitejs/devtools-kit";
import { createProcessLauncher } from "@vitejs/devtools-kit/node";
import type { Plugin } from "vite";
import { findFreePort, waitForServer, WASP_COMMAND_BIN } from "./common";

const PRISMA_STUDIO_ID = "wasp-prisma-studio";

export function prismaStudio(): Plugin {
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
          command: WASP_COMMAND_BIN,
          args: ["db", "studio", "--port", String(port)],
          env: {
            // Prisma Studio would otherwise open its own browser tab.
            BROWSER: "none",
          },
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
