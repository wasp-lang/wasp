import { createServer } from "node:net";
import { setTimeout as sleep } from "node:timers/promises";

// `wasp start` passes the path to its own executable, so the panel uses the
// same Wasp binary as the app.
export const WASP_COMMAND_BIN = process.env["WASP_EXE_PATH"] ?? "wasp";

/**
 * Finds a port that's free right now, for the panels that start a server.
 *
 * We use the mechanism where a `0` port tells the operating system to assign an
 * available port automatically.
 *
 * There's a small chance for a race condition where the port becomes occupied
 * immediately after being found free and before starting the panel's server,
 * but this is generally rare and acceptable for a local dev tool.
 */
export function findFreePort(): Promise<number> {
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
          reject(new Error("Couldn't find a free port."));
        }
      });
    });
  });
}

// Generous, since the server's first start can include installing its
// dependencies.
const WAIT_FOR_SERVER_TIMEOUT_MS = 60_000;

/**
 * Waits until `url` responds, for the panels that start a server and embed it
 * once it's up. Throws if the server's process exits first, or if the server
 * takes too long, so the launcher shows an error.
 */
export async function waitForServer(
  url: string,
  isProcessRunning: () => boolean,
): Promise<void> {
  const deadline = Date.now() + WAIT_FOR_SERVER_TIMEOUT_MS;
  while (Date.now() < deadline) {
    if (!isProcessRunning()) {
      throw new Error(`The server for ${url} exited before it was ready.`);
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
  throw new Error(`The server for ${url} didn't start in time.`);
}
