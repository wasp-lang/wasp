import { createServer } from "node:net";
import { setTimeout as sleep } from "node:timers/promises";

import type { DevToolsChildProcessTerminalSession } from "@vitejs/devtools-kit";
import { createProcessLauncher } from "@vitejs/devtools-kit/node";
import type { Plugin } from "vite";

const PRISMA_STUDIO_ID = "wasp-prisma-studio";

// Generous, since Prisma Studio's first start can take a while.
const SERVER_TIMEOUT_MS = 60_000;

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

function findFreePort(): Promise<number> {
  return new Promise((resolvePort, reject) => {
    const server = createServer();
    server.unref();
    server.on("error", reject);
    server.listen(0, () => {
      const address = server.address();
      server.close(() => {
        if (address && typeof address === "object") {
          resolvePort(address.port);
        } else {
          reject(new Error("Couldn't find a free port for Prisma Studio."));
        }
      });
    });
  });
}

/**
 * Waits until Prisma Studio responds at `url`. Throws if its process exits
 * first, or if it takes too long, so the launcher shows an error.
 */
async function waitForServer(
  url: string,
  isProcessRunning: () => boolean,
): Promise<void> {
  const deadline = Date.now() + SERVER_TIMEOUT_MS;
  while (Date.now() < deadline) {
    if (!isProcessRunning()) {
      throw new Error("Prisma Studio exited before it was ready.");
    }
    try {
      const response = await fetch(url);
      if (response.ok) {
        return;
      }
    } catch {
      // Not listening yet.
    }
    await sleep(250);
  }
  throw new Error("Prisma Studio didn't start in time.");
}
