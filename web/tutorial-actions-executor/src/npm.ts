import { $ } from "zx";

import type { AppDirPath } from "./tutorialApp.ts";

export async function npmInstallPackages({
  appDir,
  packageSpecifiers,
}: {
  appDir: AppDirPath;
  packageSpecifiers: string[];
}): Promise<void> {
  await $({ cwd: appDir })`npm install ${packageSpecifiers}`;
}
