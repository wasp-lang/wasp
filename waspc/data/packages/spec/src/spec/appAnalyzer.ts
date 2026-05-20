import * as AppSpec from "../appSpec.js";
import type { Result } from "../result.js";
import { loadWaspTsSpecDefaultExport } from "../spec-pipeline/loadWaspTsSpec.js";
import { mapApp } from "./mapApp.js";
import * as TsAppSpec from "./publicApi/tsAppSpec.js";

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
}): Promise<Result<AppSpec.Decl[], string>> {
  const waspTsDefaultExport = await loadWaspTsSpecDefaultExport({
    specPath: waspTsSpecPath,
    tsconfigPath,
    projectRootDir,
  });

  const appResult = getApp(waspTsDefaultExport);

  if (appResult.status === "error") {
    return appResult;
  }

  const app = appResult.value;
  const appSpecDecls = mapApp(app, entityNames);

  return {
    status: "ok",
    value: appSpecDecls,
  };
}

function getApp(waspTsDefaultExport: unknown): Result<TsAppSpec.App, string> {
  if (!waspTsDefaultExport) {
    return {
      status: "error",
      error:
        "Could not load your app config. " +
        "Make sure your *.wasp.ts file includes a default export of the app.",
    };
  }

  if (!isApp(waspTsDefaultExport)) {
    return {
      status: "error",
      error:
        "The default export of your *.wasp.ts file must be the result of " +
        "calling `app({ ... })`. Make sure you export that value.",
    };
  }

  return { status: "ok", value: waspTsDefaultExport };
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
    "parts" in value
  );
}
