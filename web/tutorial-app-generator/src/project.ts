import path from "path";
import type {
  AppDirPath,
  AppName,
  PatchesDirPath,
  TutorialDirPath,
} from "./brandedTypes";

export const appName = "TodoApp" as AppName;
export const appDir = path.resolve(".", appName) as AppDirPath;
export const tutorialDir = path.resolve("../docs/tutorial") as TutorialDirPath;
export const patchesDir = path.resolve(
  tutorialDir,
  "patches",
) as PatchesDirPath;
export const mainBranchName = "main";
