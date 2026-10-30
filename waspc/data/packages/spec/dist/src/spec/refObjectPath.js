import { realpathSync } from "node:fs";
import * as path from "node:path/posix"; // Module paths are always `/`-delimited
import { WaspSpecUserError } from "./waspSpecUserError.js";
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
export function normalizeRefObjectPath({ importPath, importingFilePath, projectRootDir, }) {
    const srcRelativePath = getValidSrcRelativePath({
        importPath,
        importingFilePath,
        projectRootDir,
    });
    return toAppSpecExtImportPath(srcRelativePath);
}
function getValidSrcRelativePath({ importPath, importingFilePath, projectRootDir, }) {
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
        throw new WaspSpecUserError(`Reference import path ${JSON.stringify(importPath)} in ${JSON.stringify(importingFilePath)} must resolve to a file inside the app src/ directory.`);
    }
    return srcRelativePath;
}
function getCanonicalPath(filePath) {
    try {
        return realpathSync(filePath);
    }
    catch (error) {
        if (isNodeError(error) && error.code === "ENOENT") {
            return filePath;
        }
        throw error;
    }
}
function toCanonicalProjectPath({ filePath, projectRootPath, canonicalProjectRootPath, }) {
    const projectRelativePath = getPathInsideRoot(filePath, projectRootPath) ??
        getPathInsideRoot(filePath, canonicalProjectRootPath);
    return projectRelativePath !== undefined
        ? path.resolve(canonicalProjectRootPath, projectRelativePath)
        : filePath;
}
function getPathInsideRoot(filePath, rootDir) {
    const relativePath = path.relative(rootDir, filePath);
    return isInsideRootRelativePath(relativePath) ? relativePath : undefined;
}
function toAppSpecExtImportPath(srcRelativePath) {
    return path.join("@src", srcRelativePath);
}
function isValidSrcRelativeFilePath(srcRelativePath) {
    return srcRelativePath !== "" && isInsideRootRelativePath(srcRelativePath);
}
function isInsideRootRelativePath(relativePath) {
    return (!startsWithParentSegment(relativePath) && !path.isAbsolute(relativePath));
}
function startsWithParentSegment(filePath) {
    return filePath === ".." || filePath.startsWith(`..${path.sep}`);
}
function isNodeError(error) {
    return error instanceof Error && "code" in error;
}
