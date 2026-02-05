import fs from "fs";
import path from "node:path";

import { WaspProjectDir } from "./brandedTypes.js";
import { getClientBuildDir } from "./waspProject.js";

export function isSsrEnabled(waspProjectDir: WaspProjectDir): boolean {
  const ssrConfigPath = path.join(getClientBuildDir(waspProjectDir), "ssr.json");
  if (!fs.existsSync(ssrConfigPath)) {
    return false;
  }

  try {
    const raw = fs.readFileSync(ssrConfigPath, "utf8");
    const parsed = JSON.parse(raw) as { enabled?: boolean };
    return parsed?.enabled === true;
  } catch {
    return false;
  }
}
