import path from "path";

import type { Branded } from "./brandedTypes";

export type AppName = Branded<string, "AppName">;
export type AppDirPath = Branded<string, "AppDirPath">;
export type AppParentDirPath = Branded<string, "AppParentDirPath">;
export type TutorialDirPath = Branded<string, "TutorialDirPath">;
export type PatchesDirPath = Branded<string, "PatchesDirPath">;

const tutorialAppName = "TodoApp" as AppName;
const tutorialAppParentDirPath = path.resolve("./.result") as AppParentDirPath;
const tutorialAppDirPath = path.resolve(
  tutorialAppParentDirPath,
  tutorialAppName,
) as AppDirPath;
const docsTutorialDirPath = path.resolve("../docs/tutorial") as TutorialDirPath;
const docsTutorialPatchesPath = path.resolve(
  docsTutorialDirPath,
  "patches",
) as PatchesDirPath;

export const tutorialApp = {
  name: tutorialAppName,
  parentDirPath: tutorialAppParentDirPath,
  dirPath: tutorialAppDirPath,
  docsTutorialDirPath,
  docsTutorialPatchesPath,
} as const;

export type TutorialApp = typeof tutorialApp;
