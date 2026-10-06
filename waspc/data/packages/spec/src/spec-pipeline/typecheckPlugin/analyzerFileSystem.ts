import * as fs from "node:fs";
import { createRequire } from "node:module";
import * as path from "node:path";
import { fileURLToPath } from "node:url";
import type { FileSystem, FileSystemEntries } from "typescript/unstable/fs";

const OWN_PACKAGE_DIR = findOwnPackageDir();
const OWN_NODE_TYPES_DIR = findOwnNodeTypesDir();

const WASP_SPEC_LINK_SUFFIX = path.join("node_modules", "@wasp.sh", "spec");
const NODE_TYPES_LINK_SUFFIX = path.join("node_modules", "@types", "node");

/**
 * The file system TypeScript sees while type checking the spec files: the real
 * one, plus a few virtual entries that never touch the disk.
 *
 * - `virtualFiles` (the spec sources as the bundler transformed them, and the
 *   analyzer's tsconfig) are read from memory.
 * - Every `node_modules/@wasp.sh/spec` TypeScript looks into, including the
 *   project's own even when its `node_modules` doesn't exist, behaves like a
 *   symlink to this package. A stale or missing copy in the project never
 *   matters, and userland libraries that import `@wasp.sh/spec` get the same
 *   types as the spec files. Since `realpath` follows the link, the package's
 *   own imports (e.g., `type-fest`) resolve from this package's dependencies,
 *   exactly as they would through npm's `file:` symlink.
 * - When TypeScript's default type roots (the project's and its ancestors'
 *   `node_modules/@types`) have no `@types/node` (e.g., a fresh clone), the
 *   project's `node_modules/@types/node` is a symlink to this package's own
 *   `@types/node`. Otherwise TypeScript finds the project's.
 *
 * Everything else falls through to the real file system, and TypeScript
 * resolves it with the user's own compiler options.
 */
export function createAnalyzerFileSystem({
  projectRootDir,
  virtualFiles,
}: {
  projectRootDir: string;
  virtualFiles: ReadonlyMap<string, string>;
}): Required<
  Pick<
    FileSystem,
    | "readFile"
    | "fileExists"
    | "directoryExists"
    | "getAccessibleEntries"
    | "realpath"
  >
> {
  const projectNodeTypesLink =
    OWN_NODE_TYPES_DIR && !hasNodeTypesInDefaultTypeRoots(projectRootDir)
      ? path.join(projectRootDir, NODE_TYPES_LINK_SUFFIX)
      : undefined;

  // Maps a path inside a virtual symlink to the real path it points to.
  function resolveVirtualSymlink(fileOrDirPath: string): string | undefined {
    const waspSpecLink = findWaspSpecLink(fileOrDirPath);
    if (waspSpecLink) {
      return OWN_PACKAGE_DIR + fileOrDirPath.slice(waspSpecLink.length);
    }
    if (
      projectNodeTypesLink &&
      isSameOrInside(fileOrDirPath, projectNodeTypesLink)
    ) {
      return (
        OWN_NODE_TYPES_DIR + fileOrDirPath.slice(projectNodeTypesLink.length)
      );
    }
    return undefined;
  }

  function findWaspSpecLink(fileOrDirPath: string): string | undefined {
    const linkIndex = fileOrDirPath.indexOf(path.sep + WASP_SPEC_LINK_SUFFIX);
    if (linkIndex === -1) return undefined;

    const link = fileOrDirPath.slice(
      0,
      linkIndex + path.sep.length + WASP_SPEC_LINK_SUFFIX.length,
    );
    const linkOwnerDir =
      fileOrDirPath.slice(0, linkIndex) || path.parse(fileOrDirPath).root;
    return isSameOrInside(fileOrDirPath, link) && hasWaspSpecLink(linkOwnerDir)
      ? link
      : undefined;
  }

  // The project always has the link, even without a `node_modules`. Any other
  // `node_modules` TypeScript might look into gets it too.
  function hasWaspSpecLink(dir: string): boolean {
    return (
      dir === projectRootDir || isRealDirectory(path.join(dir, "node_modules"))
    );
  }

  // The virtual symlinks directly inside `dir`, plus the virtual directories
  // leading to them.
  function getVirtualDirectoryNames(dir: string): string[] {
    const names: string[] = [];
    if (dir === projectRootDir) {
      names.push("node_modules");
    }
    if (
      path.basename(dir) === "node_modules" &&
      hasWaspSpecLink(path.dirname(dir))
    ) {
      names.push("@wasp.sh");
    }
    if (
      path.basename(dir) === "@wasp.sh" &&
      path.basename(path.dirname(dir)) === "node_modules" &&
      hasWaspSpecLink(path.dirname(path.dirname(dir)))
    ) {
      names.push("spec");
    }
    if (projectNodeTypesLink) {
      if (dir === path.join(projectRootDir, "node_modules")) {
        names.push("@types");
      }
      if (dir === path.dirname(projectNodeTypesLink)) {
        names.push("node");
      }
    }
    return names;
  }

  function getVirtualFileNames(dir: string): string[] {
    return [...virtualFiles.keys()]
      .filter((virtualFilePath) => path.dirname(virtualFilePath) === dir)
      .map((virtualFilePath) => path.basename(virtualFilePath));
  }

  function isVirtualDirectory(dirPath: string): boolean {
    return getVirtualDirectoryNames(path.dirname(dirPath)).includes(
      path.basename(dirPath),
    );
  }

  // Each callback answers for the virtual entries and returns `undefined` for
  // everything else, which makes TypeScript use the real file system.
  return {
    readFile(fileName) {
      const virtualFile = virtualFiles.get(fileName);
      if (virtualFile !== undefined) return virtualFile;

      const realPath = resolveVirtualSymlink(fileName);
      if (realPath === undefined) return undefined;
      // `null` means "doesn't exist", so a stale copy behind the link can't
      // show through.
      return readFileIfExists(realPath) ?? null;
    },

    fileExists(fileName) {
      if (virtualFiles.has(fileName)) return true;

      const realPath = resolveVirtualSymlink(fileName);
      if (realPath === undefined) return undefined;
      return statIfExists(realPath)?.isFile() ?? false;
    },

    directoryExists(dirPath) {
      const realPath = resolveVirtualSymlink(dirPath);
      if (realPath !== undefined) {
        return statIfExists(realPath)?.isDirectory() ?? false;
      }
      return isVirtualDirectory(dirPath) ? true : undefined;
    },

    getAccessibleEntries(dirPath) {
      const realPath = resolveVirtualSymlink(dirPath);
      if (realPath !== undefined) return readDirectoryEntries(realPath);

      const virtualFileNames = getVirtualFileNames(dirPath);
      const virtualDirectoryNames = getVirtualDirectoryNames(dirPath);
      if (virtualFileNames.length === 0 && virtualDirectoryNames.length === 0) {
        return undefined;
      }
      const realEntries = readDirectoryEntries(dirPath);
      return {
        files: union(realEntries.files, virtualFileNames),
        directories: union(realEntries.directories, virtualDirectoryNames),
      };
    },

    realpath(fileOrDirPath) {
      if (virtualFiles.has(fileOrDirPath)) return fileOrDirPath;

      const realPath = resolveVirtualSymlink(fileOrDirPath);
      if (realPath !== undefined) return realpathIfExists(realPath);
      return isVirtualDirectory(fileOrDirPath) ? fileOrDirPath : undefined;
    },
  };
}

// The places TypeScript looks for `@types` packages unless the user sets
// `typeRoots`: `node_modules/@types` in the project and in every ancestor.
function hasNodeTypesInDefaultTypeRoots(projectRootDir: string): boolean {
  for (let dir = projectRootDir; ; dir = path.dirname(dir)) {
    if (isRealDirectory(path.join(dir, NODE_TYPES_LINK_SUFFIX))) return true;
    if (path.dirname(dir) === dir) return false;
  }
}

function isSameOrInside(fileOrDirPath: string, dir: string): boolean {
  return fileOrDirPath === dir || fileOrDirPath.startsWith(dir + path.sep);
}

function union(a: string[], b: string[]): string[] {
  return [...new Set([...a, ...b])];
}

function statIfExists(fileOrDirPath: string): fs.Stats | undefined {
  return fs.statSync(fileOrDirPath, { throwIfNoEntry: false });
}

function isRealDirectory(dirPath: string): boolean {
  return statIfExists(dirPath)?.isDirectory() ?? false;
}

function readFileIfExists(filePath: string): string | undefined {
  try {
    return fs.readFileSync(filePath, "utf8");
  } catch {
    return undefined;
  }
}

function realpathIfExists(fileOrDirPath: string): string {
  try {
    return fs.realpathSync(fileOrDirPath);
  } catch {
    return fileOrDirPath;
  }
}

function readDirectoryEntries(dirPath: string): FileSystemEntries {
  const files: string[] = [];
  const directories: string[] = [];
  let dirents: fs.Dirent[];
  try {
    dirents = fs.readdirSync(dirPath, { withFileTypes: true });
  } catch {
    return { files, directories };
  }
  for (const dirent of dirents) {
    // Symlinks (e.g., npm's `file:` dependencies) count as what they point to.
    const stats = dirent.isSymbolicLink()
      ? statIfExists(path.join(dirPath, dirent.name))
      : dirent;
    if (stats?.isDirectory()) directories.push(dirent.name);
    else if (stats?.isFile()) files.push(dirent.name);
  }
  return { files, directories };
}

function findOwnPackageDir(): string {
  for (
    let dir = path.dirname(fileURLToPath(import.meta.url));
    ;
    dir = path.dirname(dir)
  ) {
    const manifestPath = path.join(dir, "package.json");
    if (
      fs.existsSync(manifestPath) &&
      JSON.parse(fs.readFileSync(manifestPath, "utf8")).name === "@wasp.sh/spec"
    ) {
      return dir;
    }
    if (path.dirname(dir) === dir) {
      throw new Error("Could not find the @wasp.sh/spec package root.");
    }
  }
}

// `@types/node` is a dependency of this package, so Node's lookup finds it
// wherever this package's dependencies are installed.
function findOwnNodeTypesDir(): string | undefined {
  try {
    return path.dirname(
      createRequire(import.meta.url).resolve("@types/node/package.json"),
    );
  } catch {
    return undefined;
  }
}
