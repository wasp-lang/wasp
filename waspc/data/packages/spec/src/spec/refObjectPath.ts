import { win32 as windowsPath } from "node:path";
import * as path from "node:path/posix";
import { WaspSpecUserError } from "./waspSpecUserError.js";

export function normalizeRefObjectPath({
  importPath,
  specFilePath,
  originKind,
}: {
  importPath: string;
  specFilePath: string;
  originKind: "project" | "package";
}): string {
  assertValidSpecFilePath(specFilePath);

  const importedFilePath = path.normalize(
    path.join(path.dirname(specFilePath), importPath),
  );
  const srcRelativePath = path.relative("src", importedFilePath);

  if (!isValidSrcRelativeFilePath(srcRelativePath)) {
    const owner = originKind === "project" ? "app" : "package";
    throw new WaspSpecUserError(
      `Reference import path ${JSON.stringify(importPath)} in ${JSON.stringify(specFilePath)} must resolve to a file inside the ${owner} src/ directory.`,
    );
  }

  return stripSourceExtension(srcRelativePath);
}

function assertValidSpecFilePath(specFilePath: string): void {
  const normalizedPath = path.normalize(specFilePath);
  if (
    specFilePath.length === 0 ||
    path.isAbsolute(specFilePath) ||
    windowsPath.isAbsolute(specFilePath) ||
    normalizedPath === ".." ||
    normalizedPath.startsWith("../")
  ) {
    throw new WaspSpecUserError(
      `Spec file path ${JSON.stringify(specFilePath)} must be relative to its project or package root.`,
    );
  }
}

function stripSourceExtension(filePath: string): string {
  return filePath.replace(/\.(tsx?|jsx?)$/, "");
}

function isValidSrcRelativeFilePath(srcRelativePath: string): boolean {
  return (
    srcRelativePath !== "" &&
    srcRelativePath !== ".." &&
    !srcRelativePath.startsWith("../") &&
    !path.isAbsolute(srcRelativePath)
  );
}
