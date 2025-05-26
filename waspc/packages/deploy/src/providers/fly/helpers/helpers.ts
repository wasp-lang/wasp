import { Command } from "commander";
import fs from "fs";
import path from "node:path";
import { exit } from "process";
import {
  ensureWaspProjectDirInCmdIsAbsoluteAndPresent,
  waspSays,
} from "../../../helpers.js";

export function ensureDirsInFlyCmdAreAbsoluteAndPresent(
  thisCommand: Command,
): void {
  ensureWaspProjectDirInCmdIsAbsoluteAndPresent(thisCommand);
  ensureFlyTomlDirInCmdIsAbsoluteAndPresent(thisCommand);
}

function ensureFlyTomlDirInCmdIsAbsoluteAndPresent(thisCommand: Command): void {
  const flyTomlDirPath: string | undefined = thisCommand.opts().flyTomlDir;
  if (flyTomlDirPath) {
    if (!path.isAbsolute(flyTomlDirPath)) {
      waspSays("The toml dir path must be absolute.");
      exit(1);
    }

    const flyTomlDirExists = fs.existsSync(flyTomlDirPath);
    if (!flyTomlDirExists) {
      waspSays("The toml dir path does not exist.");
      exit(1);
    }
  }
}
