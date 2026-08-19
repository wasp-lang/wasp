import * as z from "zod";

/**
 * Clerk's publishable key has to reach the browser, so it goes through Wasp's
 * client env schema rather than `import.meta.env` -- that keeps it typed and
 * validated at startup instead of failing at render time.
 */
export const clientEnvSchema = z.object({
  REACT_APP_CLERK_PUBLISHABLE_KEY: z.string().min(1),
});
