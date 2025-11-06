import type { Action } from "../actions/actions.js";
import type { TutorialApp } from "../tutorialApp.js";
import { getTutorialFilePaths } from "./fileOperations.js";
import { getActionsFromMdxFile } from "./mdxParsing.js";

export async function getActionsFromTutorialFiles(
  tutorialApp: TutorialApp,
): Promise<Action[]> {
  const tutorialFilePaths = await getTutorialFilePaths(
    tutorialApp.docsTutorialDirPath,
  );
  const actions: Action[] = [];

  for (const filePath of tutorialFilePaths) {
    const fileActions = await getActionsFromMdxFile(
      filePath,
      tutorialApp.docsTutorialPatchesPath,
    );
    actions.push(...fileActions);
  }

  return actions;
}
