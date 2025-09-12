import { $, fs } from "zx";

import { initGitRepo } from "../git";
import { log } from "../log";
import {
  type AppDirPath,
  type AppName,
  type AppParentDirPath,
} from "../tutorialApp";
import { waspNew } from "../waspCli";

export async function initApp({
  tutorialAppName,
  tutorialAppDirPath,
  tutorialAppParentDirPath,
  mainBranchName,
}: {
  tutorialAppName: AppName;
  tutorialAppParentDirPath: AppParentDirPath;
  tutorialAppDirPath: AppDirPath;
  mainBranchName: string;
}): Promise<void> {
  await fs.ensureDir(tutorialAppParentDirPath);
  await $`rm -rf ${tutorialAppDirPath}`;
  await waspNew({
    appName: tutorialAppName,
    appParentDir: tutorialAppParentDirPath,
  });
  await initGitRepo(tutorialAppDirPath, mainBranchName);
  log("info", `Tutorial app has been initialized in ${tutorialAppDirPath}`);
}
