import path from "path";

import type { Branded } from "./brandedTypes";

export type AppName = Branded<string, "AppName">;
export type AppDirPath = Branded<string, "AppDirPath">;
export type AppParentDirPath = Branded<string, "AppParentDirPath">;
export type TutorialDirPath = Branded<string, "TutorialDirPath">;
export type PatchesDirPath = Branded<string, "PatchesDirPath">;

export const tutorialAppName = "TodoApp" as AppName;
export const tutorialAppParentDirPath = path.resolve(
  "./.generated-apps",
) as AppParentDirPath;
export const tutorialAppDirPath = path.resolve(
  tutorialAppParentDirPath,
  tutorialAppName,
) as AppDirPath;
export const docsTutorialDirPath = path.resolve(
  "../docs/tutorial",
) as TutorialDirPath;
export const docsTutorialPatchesPath = path.resolve(
  docsTutorialDirPath,
  "patches",
) as PatchesDirPath;
