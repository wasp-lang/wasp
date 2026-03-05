import fs from "fs/promises";
import path from "path";

import type { MdxFilePath } from "../actions/actions.js";
import type { TutorialDirPath } from "../tutorialApp.js";

const SUPPORTED_TUTORIAL_FILE_EXTENSIONS = ["md", "mdx"] as const;

export async function getTutorialFilePaths(
  tutorialDir: TutorialDirPath,
): Promise<MdxFilePath[]> {
  const files = await fs.readdir(tutorialDir);
  return sortFileNamesByNumberedPrefix(filterValidTutorialFileNames(files)).map(
    (file) => path.resolve(tutorialDir, file) as MdxFilePath,
  );
}

export function filterValidTutorialFileNames(fileNames: string[]): string[] {
  return fileNames.filter((fileName) => {
    const lowerFileName = fileName.toLowerCase();
    return SUPPORTED_TUTORIAL_FILE_EXTENSIONS.some((ext) =>
      lowerFileName.endsWith(`.${ext}`),
    );
  });
}

/**
 * Sorts a list of files which are named "01-something.md" by their numeric prefix.
 */
export function sortFileNamesByNumberedPrefix(fileNames: string[]): string[] {
  return fileNames.sort((a, b) => {
    const aNumber = parseInt(a.split("-")[0]!, 10);
    const bNumber = parseInt(b.split("-")[0]!, 10);
    return aNumber - bNumber;
  });
}
