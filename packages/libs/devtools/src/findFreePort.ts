import { createServer } from "node:net";

/**
 * Finds a port that's free right now, for the panels that start a server.
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
