import * as AppSpec from "../appSpec.js";
import { mapTsAppSpecToAppSpecDecls } from "./mapTsAppSpecToAppSpecDecls.js";
import * as TsAppSpec from "./publicApi/tsAppSpec.js";

export async function analyzeApp(
  waspTsSpecPath: string,
  entityNames: string[],
): Promise<Result<AppSpec.Decl[], string>> {
  const specResult = await getSpec(waspTsSpecPath);

  if (specResult.status === "error") {
    return specResult;
  }

  const tsAppSpec = specResult.value;
  const appSpecDecls = mapTsAppSpecToAppSpecDecls(tsAppSpec, entityNames);

  return {
    status: "ok",
    value: appSpecDecls,
  };
}

async function getSpec(
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

  if (!isTsAppSpec(usersDefaultExport)) {
    return {
      status: "error",
      error:
        "The default export of your *.wasp.ts file must be the result of " +
        "calling `app({ ... })`. Make sure you export that value.",
    };
  }

  return { status: "ok", value: usersDefaultExport };
}

// TODO: This should probably live elsewhere
function isTsAppSpec(value: unknown): value is TsAppSpec.App {
  // TODO: Make this more robust
  // App used to be branded with a kind but we gave that up.
  return (
    typeof value === "object" &&
    value !== null &&
    "kind" in value &&
    value.kind === "app"
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
