import type { ProcessExit, ProcessHandle } from "./process.js";
import { describeExit, isSuccessfulExit } from "./process.js";

/**
 * Something we start and must tear down. `exited` resolves if it dies on its
 * own; `null` means it can never die unexpectedly (e.g. SQLite, which is just
 * env vars and a no-op disposable).
 */
export interface ManagedService extends AsyncDisposable {
  readonly name: string;
  readonly exited: Promise<ProcessExit> | null;
}

export class AppFailedError extends Error {
  readonly exit: ProcessExit;
  constructor(exit: ProcessExit) {
    super(`The app ${describeExit(exit)}`);
    this.name = "AppFailedError";
    this.exit = exit;
  }
}

export class ServiceFailedError extends Error {
  readonly serviceName: string;
  readonly exit: ProcessExit;
  constructor(serviceName: string, exit: ProcessExit) {
    super(
      `Service "${serviceName}" ${describeExit(exit)} while the app was running`,
    );
    this.name = "ServiceFailedError";
    this.serviceName = serviceName;
    this.exit = exit;
  }
}

type RaceResult =
  | { source: "app"; exit: ProcessExit }
  | { source: "service"; service: ManagedService; exit: ProcessExit }
  | { source: "abort" };

/**
 * Blocks until the app stops, a supporting service dies, or the run is aborted.
 *
 * Returns normally on graceful abort or a clean app exit; throws otherwise.
 * Every racer is a resolve-only `exited` promise (or the abort signal), so the
 * race itself can never reject. Teardown is the caller's job (via `await using`).
 */
export async function waitUntilAppStops({
  app,
  services,
  signal,
}: {
  app: ProcessHandle;
  services: ManagedService[];
  signal: AbortSignal;
}): Promise<void> {
  const racers: Promise<RaceResult>[] = [
    app.exited.then((exit) => ({ source: "app", exit })),
    abortPromise(signal).then(() => ({ source: "abort" }) as RaceResult),
  ];

  for (const service of services) {
    if (service.exited !== null) {
      racers.push(
        service.exited.then((exit) => ({ source: "service", service, exit })),
      );
    }
  }

  const result = await Promise.race(racers);

  if (result.source === "abort" || signal.aborted) {
    return;
  }
  if (result.source === "service") {
    throw new ServiceFailedError(result.service.name, result.exit);
  }
  if (!isSuccessfulExit(result.exit)) {
    throw new AppFailedError(result.exit);
  }
}

function abortPromise(signal: AbortSignal): Promise<void> {
  if (signal.aborted) {
    return Promise.resolve();
  }
  return new Promise<void>((resolve) => {
    signal.addEventListener("abort", () => resolve(), { once: true });
  });
}
