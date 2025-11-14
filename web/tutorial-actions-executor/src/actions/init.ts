import { fs } from "zx";

import { initGitRepo } from "../git";
import { log } from "../log";
import {
  type AppDirPath,
  type AppName,
  type AppParentDirPath,
} from "../tutorialApp";
import { waspNew, type WaspCliCommand } from "../waspCli";

export async function initWaspAppWithGitRepo({
  waspCliCommand,
  appName,
  appDirPath,
  appParentDirPath,
  mainBranchName,
}: {
  waspCliCommand: WaspCliCommand;
  appName: AppName;
  appParentDirPath: AppParentDirPath;
  appDirPath: AppDirPath;
  mainBranchName: string;
}): Promise<void> {
  await fs.ensureDir(appParentDirPath);
  await fs.remove(appDirPath);

  await waspNew({
    waspCliCommand,
    appName,
    appParentDirPath,
  });
  await initGitRepo(appDirPath, mainBranchName);
  log("info", `Tutorial app has been initialized in ${appDirPath}`);
}
