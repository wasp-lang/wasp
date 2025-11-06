import path from "path";

import type { Branded } from "./brandedTypes";

export type AppName = Branded<string, "AppName">;
export type AppDirPath = Branded<string, "AppDirPath">;
export type AppParentDirPath = Branded<string, "AppParentDirPath">;
export type TutorialDirPath = Branded<string, "TutorialDirPath">;
export type PatchesDirPath = Branded<string, "PatchesDirPath">;

export interface TutorialApp {
  name: AppName;
  parentDirPath: AppParentDirPath;
  dirPath: AppDirPath;
  docsTutorialDirPath: TutorialDirPath;
  docsTutorialPatchesPath: PatchesDirPath;
}

export function createTutorialApp(options: {
  appName: string;
  outputDir: string;
  tutorialDir: string;
}): TutorialApp {
  const appName = options.appName as AppName;
  const parentDirPath = path.resolve(options.outputDir) as AppParentDirPath;
  const dirPath = path.resolve(parentDirPath, appName) as AppDirPath;
  const docsTutorialDirPath = path.resolve(
    options.tutorialDir,
  ) as TutorialDirPath;
  const docsTutorialPatchesPath = path.resolve(
    docsTutorialDirPath,
    "patches",
  ) as PatchesDirPath;

  return {
    name: appName,
    parentDirPath,
    dirPath,
    docsTutorialDirPath,
    docsTutorialPatchesPath,
  };
}
