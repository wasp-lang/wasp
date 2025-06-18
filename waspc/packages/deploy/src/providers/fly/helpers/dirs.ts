import fs from "fs";
import path from "node:path";
import { exit } from "process";
import { WaspProjectDir } from "../../../common/cliArgs.js";
import { waspSays } from "../../../common/terminal.js";
import { assertWaspProjectDirInCmdIsAbsoluteAndPresent } from "../../../common/waspProject.js";

export function assertDirsInFlyCmdAreAbsoluteAndPresent(
  waspProjectDir: WaspProjectDir,
  flyTomlDirPath: string | undefined,
): void {
  assertWaspProjectDirInCmdIsAbsoluteAndPresent(waspProjectDir);
  assertFlyTomlDirInCmdIsAbsoluteAndPresent(flyTomlDirPath);
}

function assertFlyTomlDirInCmdIsAbsoluteAndPresent(
  flyTomlDirPath: string | undefined,
): void {
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
