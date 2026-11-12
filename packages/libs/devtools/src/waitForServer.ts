import { setTimeout as sleep } from "node:timers/promises";

/**
 * Waits until `url` responds, for the panels that start a server and embed it
 * once it's up. Stops waiting if the server's process exits, in which case
 * the launcher reports the error.
 */
export async function waitForServer(
  url: string,
  isProcessRunning: () => boolean,
): Promise<void> {
  while (isProcessRunning()) {
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
}
