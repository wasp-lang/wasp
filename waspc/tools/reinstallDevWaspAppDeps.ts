import { execa } from "execa";
import { removeStaleWaspLockEntries } from "./waspAppPackageLock.ts";

/**
 * TODO(#2262): Once the repo's Wasp apps join the pnpm workspace, `pnpm install`
 * at the repo root replaces this.
 *
 * Repacked dev Wasp npm packages make `npm install` fail with `EINTEGRITY` on
 * their stale lockfile entries, so we drop those first.
 */
export async function reinstallDevWaspAppDeps(
  waspAppDir: string,
): Promise<void> {
  await removeStaleWaspLockEntries(waspAppDir);
  await execa({
    cwd: waspAppDir,
    stdio: "inherit",
    verbose: "short",
  })`npm install`;
}
