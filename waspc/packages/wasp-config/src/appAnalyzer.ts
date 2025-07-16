import { GET_TS_APP_SPEC } from "./_private.js";
import * as AppSpec from "./appSpec.js";
import { mapTsAppSpecToAppSpecDecls } from "./mapTsAppSpecToAppSpecDecls.js";
import { App } from "./publicApi/App.js";

export async function analyzeApp(
  waspTsSpecPath: string,
  entityNames: string[],
): Promise<Result<AppSpec.Decl[], string>> {
  const appResult = await getApp(waspTsSpecPath);

  if (appResult.status === "error") {
    return appResult;
  }

  const app = appResult.value;
  const tsAppSpec = app[GET_TS_APP_SPEC]();
  const appSpecDecls = mapTsAppSpecToAppSpecDecls(tsAppSpec, entityNames);

  return {
    status: "ok",
    value: appSpecDecls,
  };
}

async function getApp(mainWaspJs: string): Promise<Result<App, string>> {
  const usersDefaultExport: unknown = (await import(mainWaspJs)).default;

  if (!usersDefaultExport) {
    return {
      status: "error",
      error:
        "Could not load your app config. " +
        "Make sure your *.wasp.ts file includes a default export of the app.",
    };
  }

  if (!(usersDefaultExport instanceof App)) {
    return {
      status: "error",
      error:
        "The default export of your *.wasp.ts file must be an instance of App. " +
        "Make sure you export an object created with new App(...).",
    };
  }

  return { status: "ok", value: usersDefaultExport };
}

/**
 * Result type is used instead of exceptions for the normal control flow because:
 * - The error users see with the Result type is nicer (no stack trace).
 * - Exceptions can slip through the type system.
 */
type Result<Value, Error> =
  | { status: "ok"; value: Value }
  | { status: "error"; error: Error };
