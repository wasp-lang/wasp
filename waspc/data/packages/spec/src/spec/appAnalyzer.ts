import { basename } from "path";
import * as AppSpec from "../appSpec.js";
import { loadWaspTsSpecDefaultExport } from "../spec-pipeline/loadWaspTsSpec.js";
import { mapAppToAppSpecDecls } from "./mapAppToAppSpecDecls.js";
import * as TsAppSpec from "./publicApi/tsAppSpec.js";
import { SpecUserError } from "./specUserError.js";

export async function analyzeApp({
  waspTsSpecPath,
  tsconfigPath,
  projectRootDir,
  entityNames,
}: {
  waspTsSpecPath: string;
  tsconfigPath: string;
  projectRootDir: string;
  entityNames: string[];
}): Promise<AppSpec.Decl[]> {
  const waspTsDefaultExport = await loadWaspTsSpecDefaultExport({
    specPath: waspTsSpecPath,
    tsconfigPath,
    projectRootDir,
  });

  const app = getApp(basename(waspTsSpecPath), waspTsDefaultExport);

  return mapAppToAppSpecDecls(app, { entityNames, projectRootDir });
}

function getApp(
  waspTsSpecFile: string,
  waspTsDefaultExport: unknown,
): TsAppSpec.App {
  if (!waspTsDefaultExport) {
    throw new SpecUserError(
      "Could not load your app config. " +
        `Make sure '${waspTsSpecFile}' includes a default export of the app.`,
    );
  }

  if (!isApp(waspTsDefaultExport)) {
    throw new SpecUserError(
      `The default export of '${waspTsSpecFile}' file must be of type 'App'` +
        "Make sure you export the result of calling 'app({ ... })'",
    );
  }

  return waspTsDefaultExport;
}

// TODO: This should probably live elsewhere.
// TODO: Make this more robust — structural duck-typing accepts any object with
// these keys. Consider branding the return value of `app()` with a
// non-enumerable symbol and checking for it here.
function isApp(value: unknown): value is TsAppSpec.App {
  return (
    typeof value === "object" &&
    value !== null &&
    "name" in value &&
    "wasp" in value &&
    "title" in value &&
    "decls" in value
  );
}
