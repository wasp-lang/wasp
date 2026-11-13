// A fixed-window rate limit per connection, so one runaway agent can't lock
// the user out of their own app. The counts live in a RateLimitStore: in
// memory by default (exact only with a single server instance), or shared
// through the app's database (`createPrismaStore({ rateLimit })`).

/** Counts hits per key in fixed windows. */
export interface RateLimitStore {
  /**
   * Counts one hit for `key` and returns its window: the current one, or a
   * new one starting now if the last started `windowMs` or more ago.
   */
  hit(
    key: string,
    windowMs: number,
    now: Date,
  ): Promise<{ windowStart: Date; count: number }>;
}

type RateLimiter = {
  /** Null when allowed, otherwise the seconds to wait. */
  hit(key: string, now: Date): Promise<number | null>;
};

export function createRateLimiter(
  { windowMs, max }: { windowMs: number; max: number },
  store: RateLimitStore,
): RateLimiter {
  return {
    async hit(key, now) {
      const { windowStart, count } = await store.hit(key, windowMs, now);
      if (count <= max) return null;
      return Math.max(
        1,
        Math.ceil((windowStart.getTime() + windowMs - now.getTime()) / 1000),
      );
    },
  };
}

export function createMemoryRateLimitStore(): RateLimitStore {
  const windows = new Map<string, { start: number; count: number }>();
  let lastSweep = 0;
  return {
    async hit(key, windowMs, now) {
      const t = now.getTime();
      if (t - lastSweep > windowMs) {
        for (const [k, w] of windows)
          if (t - w.start >= windowMs) windows.delete(k);
        lastSweep = t;
      }
      const current = windows.get(key);
      if (!current || t - current.start >= windowMs) {
        windows.set(key, { start: t, count: 1 });
        return { windowStart: now, count: 1 };
      }
      current.count += 1;
      return { windowStart: new Date(current.start), count: current.count };
    },
  };
}
