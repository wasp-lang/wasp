import { unrun } from "unrun";
import { SpecUserError } from "../spec/specUserError.js";
import { createSpecAwareExternalResolver } from "./externalResolver.js";
import { transformWaspTsSpecFilesPlugin } from "./transformWaspTsSpecFilesPlugin/index.js";
import { typecheckPlugin } from "./typecheckPlugin/index.js";

export async function loadWaspTsSpecDefaultExport({
  specPath,
  tsconfigPath,
}: {
  specPath: string;
  tsconfigPath: string;
}): Promise<unknown> {
  const { module: specModule } = await unrun({
    path: specPath,
    inputOptions: {
      // Replaces unrun's default resolver so package spec sources get bundled
      // through the pipeline instead of being executed as-is.
      external: createSpecAwareExternalResolver(specPath),
      plugins: [
        transformWaspTsSpecFilesPlugin(),
        typecheckPlugin({ tsconfigPath }),
      ],
    },
    // By default, unrun will directly return the `default` export. We want to
    // get it ourselves, so we use this option.
    // https://gugustinette.github.io/unrun/advanced/presets.html
    preset: "bundle-require",
  }).catch((error: unknown) => {
    // When a plugin throws, the bundler wraps the original error in an
    // aggregate build error and exposes the originals on `.errors`. We dig out
    // the `SpecUserError` so it reaches the top-level handler in `run.ts` as a
    // clean user error instead of an internal crash.
    throw getSpecUserError(error) ?? error;
  });

  return getDefaultExport(specModule);
}

function getSpecUserError(error: unknown): SpecUserError | undefined {
  if (
    // Checks to convince TypeScript that we can access `error.errors[0]`.
    error instanceof Error &&
    "errors" in error &&
    Array.isArray(error.errors) &&
    error.errors.length === 1 &&
    error.errors[0] instanceof SpecUserError
  ) {
    return error.errors[0];
  } else {
    return undefined;
  }
}

function getDefaultExport(loadedModule: unknown): unknown {
  if (typeof loadedModule !== "object" || loadedModule === null) {
    return undefined;
  }

  return "default" in loadedModule ? loadedModule.default : undefined;
}
