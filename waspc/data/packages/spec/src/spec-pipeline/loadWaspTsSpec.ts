import { unrun } from "unrun";
import { lowerImportsPlugin } from "./lowerImportsPlugin.js";
import { typecheckPlugin } from "./typecheckPlugin.js";

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
    debug: true,
    inputOptions: {
      experimental: { nativeMagicString: true },
      plugins: [
        lowerImportsPlugin({ projectRootDir }),
        typecheckPlugin({ tsconfigPath }),
      ],
    },
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
