import path from "node:path";
import type { RefObject } from "../../spec/refObject.js";
import { SpecUserError } from "../../spec/specUserError.js";

export function getExtImportPathForRefImport({
  refImportPath,
  importingFilePath,
  projectRootDir,
}: {
  refImportPath: string;
  importingFilePath: string;
  projectRootDir: string;
}): RefObject["from"] {
  const srcRelativePath = getValidSrcRelativePathForRefImport({
    refImportPath,
    importingFilePath,
    projectRootDir,
  });

  return toExtImportPath(srcRelativePath);
}

/**
 * Resolve the user-written ref import from the importing spec file,
 * then express it relative to app src/.
 */
function getValidSrcRelativePathForRefImport({
  refImportPath,
  importingFilePath,
  projectRootDir,
}: {
  refImportPath: string;
  importingFilePath: string;
  projectRootDir: string;
}): string {
  const importingDir = getImportingDir(importingFilePath);
  const srcRootDir = getAppSrcDir(projectRootDir);
  const importedFilePath = path.resolve(importingDir, refImportPath);

  const srcRelativePath = path.relative(srcRootDir, importedFilePath);

  if (!isValidSrcRelativeFilePath(srcRelativePath)) {
    throw new SpecUserError(
      `Ref import ${JSON.stringify(refImportPath)} must resolve to a file inside the app src/ directory.`,
    );
  }

  return srcRelativePath;
}

/**
 * ExtImport paths use Wasp's @src alias and forward slashes on every OS.
 */
function toExtImportPath(srcRelativePath: string): RefObject["from"] {
  return `@src/${srcRelativePath.split(path.sep).join("/")}` as RefObject["from"];
}

function getImportingDir(importingFilePath: string): string {
  return path.dirname(path.resolve(importingFilePath));
}

function getAppSrcDir(projectRootDir: string): string {
  return path.resolve(projectRootDir, "src");
}

/**
 * Allows only paths inside src/.
 * path.relative returns "" for src/ itself, ".." prefixes for paths above src/,
 * and absolute paths when roots differ.
 */
function isValidSrcRelativeFilePath(srcRelativePath: string): boolean {
  return (
    srcRelativePath !== "" &&
    !startsWithParentSegment(srcRelativePath) &&
    !path.isAbsolute(srcRelativePath)
  );
}

function startsWithParentSegment(filePath: string): boolean {
  return filePath === ".." || filePath.startsWith(`..${path.sep}`);
}
