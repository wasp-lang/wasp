import path from "path";

import type {
  AppDirPath,
  AppName,
  AppParentDirPath,
  PatchesDirPath,
  TutorialDirPath,
} from "./brandedTypes";

export const appName = "TodoApp" as AppName;
export const appParentDir = path.resolve("./") as AppParentDirPath;
export const appDir = path.resolve(appParentDir, appName) as AppDirPath;
export const tutorialDir = path.resolve("../docs/tutorial") as TutorialDirPath;
export const patchesDir = path.resolve(
  tutorialDir,
  "patches",
) as PatchesDirPath;
export const mainBranchName = "main";
