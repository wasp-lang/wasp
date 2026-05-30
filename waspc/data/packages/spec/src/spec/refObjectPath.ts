import path from "node:path";
import type * as AppSpec from "../appSpec.js";
import { SpecUserError } from "./specUserError.js";

/**
 * Converts a relative ref object path from the user's `.wasp.ts` file into
 * AppSpec's absolute `@src/...` path format.
 *
 * This keeps user-authored paths source-relative while giving the Haskell side
 * the same project-rooted paths it already understands.
 *
 * For example, `./src/MainPage` from `/app/main.wasp.ts` becomes
 * `@src/MainPage`, while `./LoginPage` from `/app/src/auth/auth.wasp.ts`
 * becomes `@src/auth/LoginPage`.
 */
export function normalizeRefObjectPath({
  importPath,
  importingFilePath,
  projectRootDir,
}: {
  importPath: string;
  importingFilePath: string;
  projectRootDir: string;
}): AppSpec.ExtImport["path"] {
  const srcRelativePath = getValidSrcRelativePath({
    importPath,
    importingFilePath,
    projectRootDir,
  });

  return toAppSpecExtImportPath(srcRelativePath);
}

function getValidSrcRelativePath({
  importPath,
  importingFilePath,
  projectRootDir,
}: {
  importPath: string;
  importingFilePath: string;
  projectRootDir: string;
}): string {
  const importingDir = path.dirname(path.resolve(importingFilePath));
  const srcRootDir = path.resolve(projectRootDir, "src");
  const importedFilePath = path.resolve(importingDir, importPath);

  const srcRelativePath = path.relative(srcRootDir, importedFilePath);

  if (!isValidSrcRelativeFilePath(srcRelativePath)) {
    throw new SpecUserError(
      `Reference import path ${JSON.stringify(importPath)} in ${JSON.stringify(importingFilePath)} must resolve to a file inside the app src/ directory.`,
    );
  }

  return srcRelativePath;
}

function toAppSpecExtImportPath(
  srcRelativePath: string,
): AppSpec.ExtImport["path"] {
  return `@src/${srcRelativePath.split(path.sep).join("/")}` as AppSpec.ExtImport["path"];
}

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
