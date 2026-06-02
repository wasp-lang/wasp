import { unrun } from "unrun";
import { lowerImportsPlugin } from "./lowerImportsPlugin/index.js";
import { typecheckPlugin } from "./typecheckPlugin/index.js";

export async function loadWaspTsSpecDefaultExport({
  specPath,
  tsconfigPath,
  projectRootDir,
}: {
  specPath: string;
  tsconfigPath: string;
  projectRootDir: string;
}): Promise<unknown> {
  const { module: specModule } = await unrun({
    path: specPath,
    inputOptions: {
      plugins: [
        lowerImportsPlugin({ projectRootDir }),
        typecheckPlugin({ tsconfigPath }),
      ],
    },
    // By default, unrun will directly return the `default` export. We want to
    // get it ourselves, so we use this option.
    // https://gugustinette.github.io/unrun/advanced/presets.html
    preset: "bundle-require",
  });

  return getDefaultExport(specModule);
}

function getDefaultExport(loadedModule: unknown): unknown {
  if (typeof loadedModule !== "object" || loadedModule === null) {
    return undefined;
  }

  return "default" in loadedModule ? loadedModule.default : undefined;
}
