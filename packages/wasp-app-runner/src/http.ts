import pRetry from "p-retry";
import { createLogger } from "./logging.js";

export async function waitUntilAppReady({
  port,
  checkIntervalMs = 500,
  timeoutMs = 60_000,
}: {
  port: number;
  checkIntervalMs?: number;
  timeoutMs?: number;
}): Promise<void> {
  const logger = createLogger("wait-until-http");

  const maxAttempts = Math.floor(timeoutMs / checkIntervalMs);

  await pRetry(
    async (attempt) => {
      logger.info(
        `(${attempt}/${maxAttempts}) Checking if app on port ${port} is ready...`,
      );
      // We don't care about the response, just that we can connect.
      await fetch(`http://localhost:${port}`);
    },
    {
      factor: 1, // no exponential backoff
      minTimeout: checkIntervalMs,
      retries: maxAttempts,
    },
  );
}
