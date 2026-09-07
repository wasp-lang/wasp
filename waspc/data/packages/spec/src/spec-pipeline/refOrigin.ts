import * as path from "node:path";

export function getRootRelativeSpecFilePath(
  rootDir: string,
  filePath: string,
): string {
  const relativePath = path.relative(rootDir, filePath);
  if (
    path.isAbsolute(relativePath) ||
    relativePath === ".." ||
    relativePath.startsWith(`..${path.sep}`)
  ) {
    throw new Error(
      `Spec file ${JSON.stringify(filePath)} must be inside ${JSON.stringify(rootDir)}.`,
    );
  }

  return relativePath.replaceAll(path.sep, "/");
}
