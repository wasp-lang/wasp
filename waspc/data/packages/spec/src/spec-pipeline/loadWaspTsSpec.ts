import { randomUUID } from "node:crypto";
import * as fs from "node:fs";
import { registerHooks } from "node:module";
import * as path from "node:path";
import { pathToFileURL } from "node:url";
import { rolldown, type InputOptions } from "rolldown";
import { OWN_SPEC_ENTRY_URLS } from "../ownSpecEntryPoints.js";
import { WaspSpecUserError } from "../spec/waspSpecUserError.js";
import { resolveSpecImportsPlugin } from "./resolveSpecImportsPlugin.js";
import { transformWaspTsSpecFilesPlugin } from "./transformWaspTsSpecFilesPlugin/index.js";
import { typecheckPlugin } from "./typecheckPlugin/index.js";

export async function loadWaspTsSpecDefaultExport({
  specPath,
  tsconfigPath,
  projectRootDir,
}: {
  specPath: string;
  tsconfigPath: string;
  projectRootDir: string;
}): Promise<unknown> {
  // Rolldown's `cwd` and the source location defines need absolute paths. The
  // Wasp CLI passes absolute ones, other callers might not.
  const absSpecPath = path.resolve(specPath);
  const absTsconfigPath = path.resolve(tsconfigPath);
  const absProjectRootDir = path.resolve(projectRootDir);
  const bundleDir = getBundleDir(absProjectRootDir);

  try {
    const { bundleCode, externalUserlandPackages } = await bundleWaspTsSpec({
      specPath: absSpecPath,
      tsconfigPath: absTsconfigPath,
      projectRootDir: absProjectRootDir,
      bundleDir,
    });
    const specModule = await importBundle({
      bundleCode,
      specPath: absSpecPath,
      bundleDir,
      externalUserlandPackages,
    });
    return getDefaultExport(specModule);
  } catch (error) {
    throw findWaspSpecUserError(error) ?? error;
  }
}

async function bundleWaspTsSpec({
  specPath,
  tsconfigPath,
  projectRootDir,
  bundleDir,
}: {
  specPath: string;
  tsconfigPath: string;
  projectRootDir: string;
  bundleDir: string;
}): Promise<{ bundleCode: string; externalUserlandPackages: Set<string> }> {
  const externalUserlandPackages = new Set<string>();
  const build = await rolldown({
    input: specPath,
    cwd: projectRootDir,
    platform: "node",
    tsconfig: tsconfigPath,
    plugins: [
      resolveSpecImportsPlugin({ bundleDir, externalUserlandPackages }),
      transformWaspTsSpecFilesPlugin(),
      typecheckPlugin({ tsconfigPath, projectRootDir }),
    ],
    transform: { define: makeSourceLocationDefines(specPath) },
    logLevel: "silent",
  });

  try {
    const { output } = await build.generate({
      format: "esm",
      codeSplitting: false,
      keepNames: true,
      generatedCode: { symbols: false },
      // Rolldown doesn't write anything here, but it computes the bundle's
      // relative paths from it.
      dir: bundleDir,
    });
    return { bundleCode: output[0].code, externalUserlandPackages };
  } finally {
    await build.close();
  }
}

/**
 * The bundle runs from `bundleDir`, so we pin the source location globals to
 * the spec file. This makes them behave as if the spec file ran directly.
 */
function makeSourceLocationDefines(
  specPath: string,
): NonNullable<InputOptions["transform"]>["define"] {
  return {
    __dirname: JSON.stringify(path.dirname(specPath)),
    __filename: JSON.stringify(specPath),
    "import.meta.url": JSON.stringify(pathToFileURL(specPath).href),
    "import.meta.filename": JSON.stringify(specPath),
    "import.meta.dirname": JSON.stringify(path.dirname(specPath)),
    "import.meta.env": "process.env",
  };
}

async function importBundle({
  bundleCode,
  specPath,
  bundleDir,
  externalUserlandPackages,
}: {
  bundleCode: string;
  specPath: string;
  bundleDir: string;
  externalUserlandPackages: ReadonlySet<string>;
}): Promise<unknown> {
  // A unique name lets concurrent analyses of the same project coexist.
  const bundlePath = path.join(
    bundleDir,
    `${path.basename(specPath)}.${randomUUID()}.mjs`,
  );
  fs.mkdirSync(bundleDir, { recursive: true });
  fs.writeFileSync(bundlePath, bundleCode);

  // Userland packages stay external, so Node loads them from the project. If
  // one of them imports `@wasp.sh/spec`, it must get the analyzer's own module
  // instance too, which only a Node resolve hook can arrange. The hook exists
  // only while the bundle is imported, and only if the bundle has such imports.
  const specResolveHook =
    externalUserlandPackages.size > 0
      ? registerHooks({
          resolve(specifier, context, nextResolve) {
            const ownSpecEntryUrl = OWN_SPEC_ENTRY_URLS.get(specifier);
            return ownSpecEntryUrl
              ? { url: ownSpecEntryUrl, format: "module", shortCircuit: true }
              : nextResolve(specifier, context);
          },
        })
      : undefined;

  try {
    return await import(pathToFileURL(bundlePath).href);
  } finally {
    specResolveHook?.deregister();
    fs.rmSync(bundlePath, { force: true });
    removeDirIfEmpty(bundleDir);
  }
}

// Inside the project, so Node resolves the bundle's external imports from the
// project's `node_modules`, but outside `node_modules` itself, so Wasp's "are
// the project's dependencies installed?" check stays truthful.
function getBundleDir(projectRootDir: string): string {
  return path.join(projectRootDir, ".wasp", "spec-bundle");
}

function removeDirIfEmpty(dir: string): void {
  try {
    fs.rmdirSync(dir);
  } catch {
    // Not empty (a concurrent analysis is using it) or already gone.
  }
}

/*
 Errors can get wrapped into other errors. This walks Error.cause and
 AggregateError.errors (only if a single item) to find an underlying
 `WaspSpecUserError` if it exists.
*/
function findWaspSpecUserError(error: unknown): WaspSpecUserError | undefined {
  if (error instanceof WaspSpecUserError) {
    return error;
  }

  // Rolldown doesn't throw actual `AggregateError`s, but it adds an `errors`
  // property to the error object.
  if (
    error instanceof Error &&
    "errors" in error &&
    Array.isArray(error.errors) &&
    error.errors.length === 1
  ) {
    return findWaspSpecUserError(error.errors[0]);
  }

  if (error instanceof Error && error.cause) {
    return findWaspSpecUserError(error.cause);
  }
}

function getDefaultExport(loadedModule: unknown): unknown {
  if (typeof loadedModule !== "object" || loadedModule === null) {
    return undefined;
  }

  return "default" in loadedModule ? loadedModule.default : undefined;
}
