import { fs } from "zx";

import { initGitRepo } from "../git";
import { log } from "../log";
import { type AppDirPath, type AppName, type OutputDir } from "../tutorialApp";
import { waspNew, type WaspCliCommand } from "../waspCli";

export async function initWaspAppWithGitRepo({
  waspCliCommand,
  appName,
  appDirPath,
  outputDir,
  mainBranchName,
}: {
  waspCliCommand: WaspCliCommand;
  appName: AppName;
  outputDir: OutputDir;
  appDirPath: AppDirPath;
  mainBranchName: string;
}): Promise<void> {
  await fs.ensureDir(outputDir);
  await fs.remove(appDirPath);

  await waspNew({
    waspCliCommand,
    appName,
    outputDir,
  });
  await initGitRepo(appDirPath, mainBranchName);
  log("info", `Tutorial app has been initialized in ${appDirPath}`);
}
