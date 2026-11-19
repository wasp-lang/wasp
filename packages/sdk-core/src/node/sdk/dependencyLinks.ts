import * as fs from "node:fs";
import * as path from "node:path";

/*
The generated code in `.wasp/out` imports the packages that come with
`@wasp/wasp`, like `express` or `@wasp/sdk-core`, by their names. Node,
TypeScript and the bundlers find a package by walking up the directory tree
and looking into each `node_modules` on the way.

That walk only reaches the project's `node_modules`, and what is in there
depends on the package manager. npm puts every installed package there. pnpm
only puts the packages the project lists itself, so the generated code would
not find the rest.

So Wasp links the packages into `.wasp/out/node_modules`, which is on that
walk under every package manager.
*/

type PackageJson = {
  name: string;
  bin?: string | Record<string, string>;
  dependencies?: Record<string, string>;
  peerDependencies?: Record<string, string>;
};

const linksDirInGeneratedAppDir = "node_modules";

/** A link's path, and the path it points at. */
type Links = ReadonlyMap<string, string>;

/**
 * Links the dependencies and peer dependencies of the Wasp package, and their
 * executables, into the `node_modules` of the generated app dir.
 *
 * A package that the project lists itself is linked from the project, so the
 * generated code and the project's code share one copy of it (e.g. of React).
 */
export function linkWaspDependencies({
  waspPackageDir,
  projectDir,
  generatedAppDir,
}: {
  /** The real path of `@wasp/wasp`, next to which its dependencies are. */
  waspPackageDir: string;
  projectDir: string;
  generatedAppDir: string;
}): void {
  const linksDir = path.join(generatedAppDir, linksDirInGeneratedAppDir);
  const waspPackageJson = readPackageJson(waspPackageDir);
  const dependencyNames = [
    ...Object.keys(waspPackageJson.dependencies ?? {}),
    ...Object.keys(waspPackageJson.peerDependencies ?? {}),
  ];

  const links = new Map<string, string>();
  for (const dependencyName of dependencyNames) {
    const dependencyDir =
      findPackageDir(dependencyName, projectDir, linksDir) ??
      findPackageDir(dependencyName, waspPackageDir, linksDir);
    // An optional peer dependency is only installed when the app uses its feature.
    if (dependencyDir === undefined) {
      continue;
    }
    links.set(path.join(linksDir, dependencyName), dependencyDir);
    for (const [binName, binFile] of getBinFiles(dependencyDir)) {
      links.set(path.join(linksDir, ".bin", binName), binFile);
    }
  }

  syncLinks(linksDir, links);
}

/**
 * Whether the import is of a package that Wasp linked into the generated app
 * dir, e.g. `zod/v4`.
 *
 * Bundlers use this to leave such imports for Node to resolve when the code
 * runs. A package's path doesn't tell them that: a package manager can keep a
 * package outside of any `node_modules`, e.g. a package of the project's own
 * workspace.
 */
export function isWaspDependencyImport(
  generatedAppDir: string,
  importSpecifier: string,
): boolean {
  const packageName = getImportedPackageName(importSpecifier);
  return (
    packageName !== undefined &&
    fs.existsSync(
      path.join(generatedAppDir, linksDirInGeneratedAppDir, packageName),
    )
  );
}

/**
 * The name of the package that the import is of, e.g. `@wasp/auth` for
 * `@wasp/auth/browser`. `undefined` when it imports a file by its path.
 */
export function getImportedPackageName(
  importSpecifier: string,
): string | undefined {
  if (importSpecifier.startsWith(".") || path.isAbsolute(importSpecifier)) {
    return undefined;
  }
  const [scopeOrName, nameInScope] = importSpecifier.split("/");
  return importSpecifier.startsWith("@")
    ? `${scopeOrName}/${nameInScope}`
    : scopeOrName;
}

/**
 * Finds the package like Node does from `fromDir`, but through the file
 * system, because a package's `exports` can hide its `package.json` from
 * `require.resolve`.
 */
function findPackageDir(
  packageName: string,
  fromDir: string,
  linksDir: string,
): string | undefined {
  for (const dir of getDirAndItsAncestors(fromDir)) {
    const candidateDir = path.join(dir, "node_modules", packageName);
    // A link must not point at itself, or a stale link would never go away.
    const isOurLink = isPathInside(candidateDir, linksDir);
    if (!isOurLink && fs.existsSync(path.join(candidateDir, "package.json"))) {
      return fs.realpathSync(candidateDir);
    }
  }
  return undefined;
}

function getDirAndItsAncestors(dir: string): string[] {
  const parentDir = path.dirname(dir);
  return parentDir === dir ? [dir] : [dir, ...getDirAndItsAncestors(parentDir)];
}

function isPathInside(filePath: string, dir: string): boolean {
  return filePath.startsWith(dir + path.sep);
}

/** The package's executables: their names and files. */
function getBinFiles(packageDir: string): [string, string][] {
  const { name, bin } = readPackageJson(packageDir);
  if (bin === undefined) {
    return [];
  }
  // A lone file is an executable named like the package, without its scope.
  const binFileByName =
    typeof bin === "string" ? { [path.posix.basename(name)]: bin } : bin;
  return Object.entries(binFileByName).map(([binName, binFile]) => [
    binName,
    path.join(packageDir, binFile),
  ]);
}

/**
 * Only touches the links that changed. Vite and the server resolve packages
 * through them while Wasp recompiles the app.
 */
function syncLinks(linksDir: string, links: Links): void {
  for (const [linkPath, targetPath] of links) {
    // Relative links survive moving the project.
    const relativeTargetPath = path.relative(
      path.dirname(linkPath),
      targetPath,
    );
    if (readLinkIfExists(linkPath) === relativeTargetPath) {
      continue;
    }
    fs.mkdirSync(path.dirname(linkPath), { recursive: true });
    fs.rmSync(linkPath, { recursive: true, force: true });
    fs.symlinkSync(relativeTargetPath, linkPath);
  }

  for (const existingLinkPath of listLinks(linksDir)) {
    if (!links.has(existingLinkPath)) {
      fs.rmSync(existingLinkPath, { force: true });
    }
  }
}

/** The links are in `linksDir` itself, in scope dirs like `@wasp`, and in `.bin`. */
function listLinks(dir: string): string[] {
  if (!fs.existsSync(dir)) {
    return [];
  }
  return fs.readdirSync(dir, { withFileTypes: true }).flatMap((entry) => {
    const entryPath = path.join(dir, entry.name);
    if (entry.isSymbolicLink()) {
      return [entryPath];
    }
    return entry.isDirectory() ? listLinks(entryPath) : [];
  });
}

function readLinkIfExists(linkPath: string): string | undefined {
  try {
    return fs.readlinkSync(linkPath);
  } catch {
    return undefined;
  }
}

function readPackageJson(packageDir: string): PackageJson {
  return JSON.parse(
    fs.readFileSync(path.join(packageDir, "package.json"), "utf-8"),
  );
}
