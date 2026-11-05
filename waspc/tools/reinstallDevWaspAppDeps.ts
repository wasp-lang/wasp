import { execa } from "execa";
import { removeStaleWaspLockEntries } from "./waspAppPackageLock.ts";

/**
 * Repacked dev Wasp npm packages make `npm install` fail with `EINTEGRITY` on
 * their stale lockfile entries, so we drop those first.
 */
export async function reinstallDevWaspAppDeps(): Promise<void> {
  await removeStaleWaspLockEntries();
  await execa({ stdio: "inherit", verbose: "short" })`npm install`;
}
