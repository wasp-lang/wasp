import path from "node:path";

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
}): string {
  const srcFolder = path.resolve(projectRootDir, "src");
  const importedFilePath = path.resolve(
    path.dirname(importingFilePath),
    refImportPath,
  );
  const srcRelativePath = path.relative(srcFolder, importedFilePath);

  return path.join("@src", srcRelativePath);
}
