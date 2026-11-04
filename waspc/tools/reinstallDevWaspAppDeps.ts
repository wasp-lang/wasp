import { execa } from "execa";
import { removeStaleWaspLockEntries } from "./waspAppPackageLock.ts";

/**
 * Reinstalls the npm dependencies of the Wasp app in the current dir against
 * the dev Wasp npm packages (see `tasks/build/dev-packages.ts`).
 *
 * The dev Wasp npm packages are `file:` tarballs that change whenever their
 * sources change, and then the lockfile's entries for them go stale. With a
 * stale entry, `npm install` fails with `EINTEGRITY`, so we drop the stale
 * entries first.
 */
export async function reinstallDevWaspAppDeps(): Promise<void> {
  await removeStaleWaspLockEntries();
  await execa({ stdio: "inherit", verbose: "short" })`npm install`;
}
