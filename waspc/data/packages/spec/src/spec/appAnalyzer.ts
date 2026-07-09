import { basename } from "path";
import * as AppSpec from "../appSpec.js";
import { loadWaspTsSpecDefaultExport } from "../spec-pipeline/loadWaspTsSpec.js";
import { mapApp } from "./mapApp.js";
import * as WaspSpec from "./publicApi/waspSpec.js";
import { WaspSpecUserError } from "./waspSpecUserError.js";

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
  });

  const app = getApp(basename(waspTsSpecPath), waspTsDefaultExport);

  return mapApp(app, { entityNames, projectRootDir });
}

function getApp(
  waspTsSpecFile: string,
  waspTsDefaultExport: unknown,
): WaspSpec.App {
  if (!waspTsDefaultExport) {
    throw new WaspSpecUserError(
      "Could not load your app config. " +
        `Make sure '${waspTsSpecFile}' includes a default export of the app.`,
    );
  }

  if (!isApp(waspTsDefaultExport)) {
    throw new WaspSpecUserError(
      `The default export of '${waspTsSpecFile}' file must be of type 'App'. ` +
        "Make sure you export the result of calling 'app({ ... })'",
    );
  }

  return waspTsDefaultExport;
}

// TODO: This should probably live elsewhere.
// TODO: Make this more robust — structural duck-typing accepts any object with
// these keys. Consider branding the return value of `app()` with a
// non-enumerable symbol and checking for it here.
function isApp(value: unknown): value is WaspSpec.App {
  return (
    typeof value === "object" &&
    value !== null &&
    "name" in value &&
    "wasp" in value &&
    "title" in value &&
    "spec" in value
  );
}
