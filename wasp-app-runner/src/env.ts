import * as path from "path";
import { copyFileSync, existsSync } from "fs";
import { log } from "./logging.js";

export function setupEnvFiles({ pathToApp }: { pathToApp: string }) {
  const envFiles = [
    { headless: ".env.client.headless", target: ".env.client" },
    { headless: ".env.server.headless", target: ".env.server" },
  ];

  for (const { headless, target } of envFiles) {
    const headlessPath = path.join(pathToApp, headless);
    const targetPath = path.join(pathToApp, target);

    if (existsSync(headlessPath)) {
      log("setup", "info", `Copying ${headless} to ${target}`);
      copyFileSync(headlessPath, targetPath);
    } else {
      log("setup", "warn", `Headless env file not found: ${headless}`);
    }
  }
}
