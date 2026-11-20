import { setTimeout as sleep } from "node:timers/promises";

// Generous, since the server's first start can include installing its
// dependencies.
const TIMEOUT_MS = 60_000;

/**
 * Waits until `url` responds, for the panels that start a server and embed it
 * once it's up. Throws if the server's process exits first, or if the server
 * takes too long, so the launcher shows an error.
 */
export async function waitForServer(
  url: string,
  isProcessRunning: () => boolean,
): Promise<void> {
  const deadline = Date.now() + TIMEOUT_MS;
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
