import path from "path";
import type { AppDirPath, AppName, PatchesDirPath } from "./brandedTypes";

export const appName = "TodoApp" as AppName;
export const appDir = path.resolve(".", appName) as AppDirPath;
export const patchesDir = path.resolve(
  "../docs/tutorial",
  "patches",
) as PatchesDirPath;
export const mainBranchName = "main";
