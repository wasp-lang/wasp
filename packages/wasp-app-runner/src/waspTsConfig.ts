import { readdir } from "fs/promises";
import { PathToApp } from "./args.js";
import { createLogger } from "./logging.js";

export async function isWaspTypescriptConfigProject(pathToApp: PathToApp) {
  const logger = createLogger("wasp-ts-config");

  try {
    const files = await readdir(pathToApp);
    return files.some((file) => file.endsWith(".wasp.ts"));
  } catch (error) {
    logger.error(`Failed to read directory ${pathToApp}: ${error}`);
    process.exit(1);
  }
}
