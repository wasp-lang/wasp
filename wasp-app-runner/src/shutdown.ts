import { runExitGuardsNow } from "./exitGuard.js";
import { createLogger } from "./logging.js";

const SHUTDOWN_SIGNALS = ["SIGINT", "SIGTERM"] as const;

export class ShutdownRequestedError extends Error {
  readonly signalName: NodeJS.Signals;
  constructor(signalName: NodeJS.Signals) {
    super(`Shutdown requested via ${signalName}`);
    this.signalName = signalName;
  }
}

/**
 * Wires SIGINT/SIGTERM to a graceful, then forceful, shutdown.
 *
 * - First signal: abort the controller (let `await using` teardown run) and
 *   tell the user a second Ctrl+C forces it.
 * - Second signal: run the synchronous exit guards (group SIGKILLs +
 *   daemon-side container removal), then re-raise the signal so the kernel
 *   gives us the conventional 130/143 status. No `process.exit` involved.
 */
export function installShutdownHandlers(
  controller: AbortController,
): Disposable {
  const logger = createLogger("shutdown");
  let shuttingDown = false;

  const handlers = new Map<NodeJS.Signals, () => void>();

  function removeHandlers(): void {
    for (const [sig, handler] of handlers) {
      process.removeListener(sig, handler);
    }
    handlers.clear();
  }

  for (const sig of SHUTDOWN_SIGNALS) {
    const handler = () => {
      if (!shuttingDown) {
        shuttingDown = true;
        logger.warn(
          `Received ${sig}, shutting down gracefully. Press Ctrl+C again to force.`,
        );
        controller.abort(new ShutdownRequestedError(sig));
        return;
      }
      logger.warn(`Received ${sig} again, forcing shutdown.`);
      runExitGuardsNow();
      removeHandlers();
      // Re-raise so we die with the conventional signal exit status.
      process.kill(process.pid, sig);
    };
    handlers.set(sig, handler);
    process.on(sig, handler);
  }

  return {
    [Symbol.dispose]() {
      removeHandlers();
    },
  };
}
