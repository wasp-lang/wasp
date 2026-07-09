import { unrun } from "unrun";
import { WaspSpecUserError } from "../spec/waspSpecUserError.js";
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
    // The bundler doesn't surface the original error directly: a plugin throw
    // is wrapped in an aggregate build error (originals on `.errors`), and a
    // throw while executing the spec is wrapped with the original on `.cause`.
    // We dig out any `WaspSpecUserError` from that chain so it reaches the
    // top-level handler in `run.ts` as a clean user error instead of an
    // internal crash. This also lets userland spec libraries report errors the
    // same way the pipeline does by throwing a `WaspSpecUserError` themselves.
    throw findWaspSpecUserError(error) ?? error;
  });

  return getDefaultExport(specModule);
}

/**
 * Walks an error's `cause` and `errors` chains looking for a `WaspSpecUserError`.
 */
function findWaspSpecUserError(error: unknown): WaspSpecUserError | undefined {
  const seen = new Set<unknown>();
  const toVisit: unknown[] = [error];

  while (toVisit.length > 0) {
    const current = toVisit.pop();

    if (current instanceof WaspSpecUserError) {
      return current;
    }

    if (typeof current !== "object" || current === null || seen.has(current)) {
      continue;
    }
    seen.add(current);

    if ("cause" in current) {
      toVisit.push(current.cause);
    }
    if ("errors" in current && Array.isArray(current.errors)) {
      toVisit.push(...current.errors);
    }
  }

  return undefined;
}

function getDefaultExport(loadedModule: unknown): unknown {
  if (typeof loadedModule !== "object" || loadedModule === null) {
    return undefined;
  }

  return "default" in loadedModule ? loadedModule.default : undefined;
}
