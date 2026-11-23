import { resolve } from "node:path";

import type { DevToolsChildProcessTerminalSession } from "@vitejs/devtools-kit";
import { createProcessLauncher } from "@vitejs/devtools-kit/node";
import type { Plugin } from "vite";

import { findFreePort } from "./findFreePort";
import { waitForServer } from "./waitForServer";

const PRISMA_STUDIO_ID = "wasp-prisma-studio";

export interface PrismaStudioOptions {
  /**
   * The Prisma schema file. A relative path resolves from the project root.
   */
  schemaFile: string;
  /**
   * The directory Prisma runs from, so it reads the `.env` file there. A
   * relative path resolves from the project root.
   */
  cwd: string;
}

/**
 * Adds Prisma Studio as a panel to Vite DevTools. The panel starts Prisma
 * Studio on a free port when the user opens it, and embeds it once it's up.
 */
export function prismaStudio({ schemaFile, cwd }: PrismaStudioOptions): Plugin {
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
          command: "prisma",
          args: [
            "studio",
            "--schema",
            resolve(projectRoot, schemaFile),
            "--port",
            String(port),
            "--browser",
            "none",
          ],
          cwd: resolve(projectRoot, cwd),
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
      // `terminate` also stops the process's children, e.g. on Windows, where
      // `prisma` runs through a shell.
      server.httpServer?.once("close", () => {
        void studioSession?.terminate();
      });
    },
  };
}
