import { existsSync, readFileSync, realpathSync } from "node:fs";
import * as path from "node:path/posix"; // Module paths are always `/`-delimited
import type * as AppSpec from "../appSpec.js";
import { WaspSpecUserError } from "./waspSpecUserError.js";

/**
 * Maps a relative ref from a spec file that lives inside an installed package
 * (e.g. `node_modules/@scope/module/module.wasp.ts`) to a package import
 * source, using the module packaging convention: `./src/<subpath>` in the
 * package maps to the `<packageName>/<subpath>` package export.
 *
 * Returns `undefined` when the importing file belongs to the project itself.
 */
export function tryMapPackageRefObjectPath({
  importPath,
  importingFilePath,
  projectRootDir,
}: {
  importPath: string;
  importingFilePath: string;
  projectRootDir: string;
}): AppSpec.PackageExtImportSource | undefined {
  const canonicalImportingFilePath = getCanonicalPath(
    path.resolve(importingFilePath),
  );
  const packageRootDir = findEnclosingPackageRootDir(
    path.dirname(canonicalImportingFilePath),
  );
  if (packageRootDir === undefined) {
    return undefined;
  }

  const canonicalProjectRootPath = getCanonicalPath(
    path.resolve(projectRootDir),
  );
  if (packageRootDir === canonicalProjectRootPath) {
    return undefined;
  }

  const packageName = readPackageName(packageRootDir);
  if (packageName === undefined) {
    return undefined;
  }

  return {
    kind: "package",
    packageName,
    subpath: getValidPackageSrcSubpath({
      importPath,
      canonicalImportingFilePath,
      packageRootDir,
    }),
  };
}

function findEnclosingPackageRootDir(startDir: string): string | undefined {
  let currentDir = startDir;
  while (true) {
    if (existsSync(path.join(currentDir, "package.json"))) {
      return currentDir;
    }

    const parentDir = path.dirname(currentDir);
    if (parentDir === currentDir) {
      return undefined;
    }
    currentDir = parentDir;
  }
}

function readPackageName(packageRootDir: string): string | undefined {
  try {
    const packageJson: unknown = JSON.parse(
      readFileSync(path.join(packageRootDir, "package.json"), "utf8"),
    );

    return typeof packageJson === "object" &&
      packageJson !== null &&
      "name" in packageJson &&
      typeof packageJson.name === "string" &&
      packageJson.name.length > 0
      ? packageJson.name
      : undefined;
  } catch {
    return undefined;
  }
}

function getValidPackageSrcSubpath({
  importPath,
  canonicalImportingFilePath,
  packageRootDir,
}: {
  importPath: string;
  canonicalImportingFilePath: string;
  packageRootDir: string;
}): string {
  const importingDir = path.dirname(canonicalImportingFilePath);
  const packageSrcDir = path.resolve(packageRootDir, "src");
  const importedFilePath = path.resolve(importingDir, importPath);

  const srcRelativePath = path.relative(packageSrcDir, importedFilePath);

  if (!isValidSrcRelativeFilePath(srcRelativePath)) {
    throw new WaspSpecUserError(
      `Reference import path ${JSON.stringify(importPath)} in ${JSON.stringify(canonicalImportingFilePath)} must resolve to a file inside the package src/ directory.`,
    );
  }

  return stripSourceExtension(srcRelativePath);
}

function stripSourceExtension(filePath: string): string {
  return filePath.replace(/\.(tsx?|jsx?)$/, "");
}

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
}): AppSpec.ProjectSrcExtImportSource["path"] {
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
  // The bundler resolves symlinks in module ids, so we compare paths under a
  // canonical project root. Otherwise valid in-src imports can look like they
  // escape src/ because one path has symlinks resolved and the other does not.
  const projectRootPath = path.resolve(projectRootDir);
  const canonicalProjectRootPath = getCanonicalPath(projectRootPath);
  const canonicalImportingFilePath = toCanonicalProjectPath({
    filePath: path.resolve(importingFilePath),
    projectRootPath,
    canonicalProjectRootPath,
  });

  const importingDir = path.dirname(canonicalImportingFilePath);
  const srcRootDir = path.resolve(canonicalProjectRootPath, "src");
  const importedFilePath = path.resolve(importingDir, importPath);

  const srcRelativePath = path.relative(srcRootDir, importedFilePath);

  if (!isValidSrcRelativeFilePath(srcRelativePath)) {
    throw new WaspSpecUserError(
      `Reference import path ${JSON.stringify(importPath)} in ${JSON.stringify(importingFilePath)} must resolve to a file inside the app src/ directory.`,
    );
  }

  return srcRelativePath;
}

function getCanonicalPath(filePath: string): string {
  try {
    return realpathSync(filePath);
  } catch (error) {
    if (isNodeError(error) && error.code === "ENOENT") {
      return filePath;
    }

    throw error;
  }
}

function toCanonicalProjectPath({
  filePath,
  projectRootPath,
  canonicalProjectRootPath,
}: {
  filePath: string;
  projectRootPath: string;
  canonicalProjectRootPath: string;
}): string {
  const projectRelativePath =
    getPathInsideRoot(filePath, projectRootPath) ??
    getPathInsideRoot(filePath, canonicalProjectRootPath);

  return projectRelativePath !== undefined
    ? path.resolve(canonicalProjectRootPath, projectRelativePath)
    : filePath;
}

function getPathInsideRoot(
  filePath: string,
  rootDir: string,
): string | undefined {
  const relativePath = path.relative(rootDir, filePath);

  return isInsideRootRelativePath(relativePath) ? relativePath : undefined;
}

function toAppSpecExtImportPath(
  srcRelativePath: string,
): AppSpec.ProjectSrcExtImportSource["path"] {
  return path.join(
    "@src",
    srcRelativePath,
  ) as AppSpec.ProjectSrcExtImportSource["path"];
}

function isValidSrcRelativeFilePath(srcRelativePath: string): boolean {
  return srcRelativePath !== "" && isInsideRootRelativePath(srcRelativePath);
}

function isInsideRootRelativePath(relativePath: string): boolean {
  return (
    !startsWithParentSegment(relativePath) && !path.isAbsolute(relativePath)
  );
}

function startsWithParentSegment(filePath: string): boolean {
  return filePath === ".." || filePath.startsWith(`..${path.sep}`);
}

function isNodeError(error: unknown): error is NodeJS.ErrnoException {
  return error instanceof Error && "code" in error;
}
