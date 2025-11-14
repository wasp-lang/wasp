import path from "path";

import type { Branded } from "./brandedTypes";

export type AppName = Branded<string, "AppName">;
export type AppDirPath = Branded<string, "AppDirPath">;
export type OutputDir = Branded<string, "OutputDir">;
export type TutorialDirPath = Branded<string, "TutorialDirPath">;
export type PatchesDirPath = Branded<string, "PatchesDirPath">;

export interface TutorialApp {
  name: AppName;
  appDirPath: AppDirPath;
  outputDir: OutputDir;
  docsTutorialDirPath: TutorialDirPath;
  docsTutorialPatchesPath: PatchesDirPath;
}

export function createTutorialApp(options: {
  appName: string;
  outputDir: string;
  tutorialDir: string;
}): TutorialApp {
  const appName = options.appName as AppName;
  const outputDir = path.resolve(options.outputDir) as OutputDir;
  const appDirPath = path.resolve(outputDir, appName) as AppDirPath;
  const docsTutorialDirPath = path.resolve(
    options.tutorialDir,
  ) as TutorialDirPath;
  const docsTutorialPatchesPath = path.resolve(
    docsTutorialDirPath,
    "patches",
  ) as PatchesDirPath;

  return {
    name: appName,
    outputDir,
    appDirPath,
    docsTutorialDirPath,
    docsTutorialPatchesPath,
  };
}
