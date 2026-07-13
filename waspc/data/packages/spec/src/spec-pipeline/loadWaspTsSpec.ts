import { realpathSync } from "node:fs";
import * as path from "node:path";
import { unrun } from "unrun";
import { createWaspTsSpecPlugins } from "../compiler.js";
import { WaspSpecUserError } from "../spec/waspSpecUserError.js";

export async function loadWaspTsSpecDefaultExport({
  specPath,
  tsconfigPath,
  projectRootDir,
}: {
  specPath: string;
  tsconfigPath: string;
  projectRootDir: string;
}): Promise<unknown> {
  const canonicalProjectRootDir = realpathSync(projectRootDir);
  const { module: specModule } = await unrun({
    path: specPath,
    inputOptions: {
      plugins: createWaspTsSpecPlugins({
        tsconfigPath,
        getRefOrigin: (filePath) => {
          const specFilePath = path.relative(
            canonicalProjectRootDir,
            realpathSync(filePath),
          );
          if (
            path.isAbsolute(specFilePath) ||
            specFilePath === ".." ||
            specFilePath.startsWith(`..${path.sep}`)
          ) {
            throw new Error(
              `Project spec file ${JSON.stringify(filePath)} must be inside ${JSON.stringify(projectRootDir)}.`,
            );
          }

          return {
            kind: "project",
            specFilePath: specFilePath.replaceAll(path.sep, "/"),
          };
        },
      }),
    },
    // By default, unrun will directly return the `default` export. We want to
    // get it ourselves, so we use this option.
    // https://gugustinette.github.io/unrun/advanced/presets.html
    preset: "bundle-require",
  }).catch((error: unknown) => {
    throw findWaspSpecUserError(error) ?? error;
  });

  return getDefaultExport(specModule);
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

  // unrun doesn't throw actual `AggregateError`s, but it adds an `errors`
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
