import path from "node:path";
import { SpecUserError } from "../../spec/specUserError.js";

export type RefImportPath = `@src/${string}`;

/**
 * Resolves a ref import path (as written in the importing spec file) to an
 * `@src/...` path, which is how Wasp references files in the app's `src/`
 * directory.
 */
export function mapImportPath({
  refImportPath,
  importingFilePath,
  projectRootDir,
}: {
  refImportPath: string;
  importingFilePath: string;
  projectRootDir: string;
}): RefImportPath {
  const srcFolder = path.resolve(projectRootDir, "src");
  const importedFilePath = path.resolve(
    path.dirname(importingFilePath),
    refImportPath,
  );
  const srcRelativePath = path.relative(srcFolder, importedFilePath);

  if (!isInsideSrc(srcRelativePath)) {
    throw new SpecUserError(
      `Ref import ${JSON.stringify(refImportPath)} must resolve to a file inside the app src/ directory.`,
    );
  }

  return path.join("@src", srcRelativePath) as RefImportPath;
}

function isInsideSrc(srcRelativePath: string): boolean {
  return (
    srcRelativePath !== "" &&
    srcRelativePath !== ".." &&
    !srcRelativePath.startsWith(`..${path.sep}`) &&
    !path.isAbsolute(srcRelativePath)
  );
}
