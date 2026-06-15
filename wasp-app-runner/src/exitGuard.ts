/**
 * Last-resort, synchronous-only cleanup that runs when the process is about to
 * die in a way that skips normal async teardown (uncaught exception, or a
 * forced second SIGINT/SIGTERM).
 *
 * Guards MUST be synchronous: the Node `exit` event does not wait for promises,
 * and the forced-shutdown path calls {@link runExitGuardsNow} explicitly right
 * before re-raising the killing signal.
 */

const guards = new Set<() => void>();
let installed = false;

export function registerExitGuard(guard: () => void): () => void {
  ensureInstalled();
  guards.add(guard);
  return () => {
    guards.delete(guard);
  };
}

export function runExitGuardsNow(): void {
  // Copy then clear so each guard runs at most once, even if this is called
  // explicitly and then again from the `exit` event.
  const toRun = [...guards];
  guards.clear();
  for (const guard of toRun) {
    try {
      guard();
    } catch {
      // A last-resort cleanup guard must never throw.
    }
  }
}

function ensureInstalled(): void {
  if (installed) {
    return;
  }
  installed = true;
  // Covers exits we don't drive explicitly (e.g. an uncaught exception).
  // Signal-driven death does NOT run `exit` handlers, which is why the
  // forced-shutdown path calls `runExitGuardsNow()` itself.
  process.on("exit", runExitGuardsNow);
}
