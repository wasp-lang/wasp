import path from "node:path";
import { type Plugin } from "vite";
import { ENVIRONMENT_NAMES } from "../../../vite/constants.js";

export function detectClientImports(): Plugin {
  let parsePathToUserCode!: ParsePathToUserCodeFn;

  return {
    name: "wasp:detect-client-imports",
    enforce: "pre",
    // Importing client code is only forbidden in the environment that processes
    // server code.
    applyToEnvironment: (environment) =>
      environment.name === ENVIRONMENT_NAMES.SERVER,
    configResolved(config) {
      parsePathToUserCode = createPathToUserCodeParser(config.root);
    },
    resolveId(source, importer) {
      if (!importer) {
        return;
      }

      const pathToUserCode = parsePathToUserCode(importer);
      if (!pathToUserCode) {
        return;
      }

      if (isClientImport(source)) {
        throw new Error(
          `Client code cannot be imported in the server code. Import from "${source}" in "${pathToUserCode}" is not allowed.`,
        );
      }
    },
  };
}

function isClientImport(moduleName: string): boolean {
  return moduleName.startsWith("wasp/client");
}

type RelativePathToUserCode = string & { _brand: "relativePathToUserCode" };

type ParsePathToUserCodeFn = (
  importerPath: string,
) => RelativePathToUserCode | null;

function createPathToUserCodeParser(
  waspProjectDirPath: string,
): ParsePathToUserCodeFn {
  return (importerPath: string): RelativePathToUserCode | null => {
    const importerPathRelativeToWaspProjectDir = path.relative(
      waspProjectDirPath,
      importerPath,
    );
    return importerPathRelativeToWaspProjectDir.startsWith(
      "src/",
    )
      ? (importerPathRelativeToWaspProjectDir as RelativePathToUserCode)
      : null;
  };
}
