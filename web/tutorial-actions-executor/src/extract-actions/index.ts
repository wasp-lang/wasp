import type { Action } from "../actions/actions.ts";
import type { TutorialApp } from "../tutorialApp.ts";
import { getTutorialFilePaths } from "./fileOperations.ts";
import { getActionsFromMdxFile } from "./mdxParsing.ts";

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
