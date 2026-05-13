import * as AppSpec from "../appSpec.js";
import { mapApp } from "./mapApp.js";
import * as TsAppSpec from "./publicApi/tsAppSpec.js";

export async function analyzeApp(
  waspTsSpecPath: string,
  entityNames: string[],
): Promise<Result<AppSpec.Decl[], string>> {
  const appResult = await getApp(waspTsSpecPath);

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

async function getApp(
  mainWaspJs: string,
): Promise<Result<TsAppSpec.App, string>> {
  const usersDefaultExport: unknown = await (await import(mainWaspJs)).default;

  if (!usersDefaultExport) {
    return {
      status: "error",
      error:
        "Could not load your app config. " +
        "Make sure your *.wasp.ts file includes a default export of the app.",
    };
  }

  if (!isApp(usersDefaultExport)) {
    return {
      status: "error",
      error:
        "The default export of your *.wasp.ts file must be the result of " +
        "calling `app({ ... })`. Make sure you export that value.",
    };
  }

  return { status: "ok", value: usersDefaultExport };
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

/**
 * Result type is used instead of exceptions for the normal control flow because:
 * - The error users see with the Result type is nicer (no stack trace).
 * - Exceptions can slip through the type system.
 */
type Result<Value, Error> =
  | { status: "ok"; value: Value }
  | { status: "error"; error: Error };
