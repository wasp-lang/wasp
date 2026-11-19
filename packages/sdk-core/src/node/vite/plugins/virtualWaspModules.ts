import type { Plugin } from "vite";
import {
  readGeneratedVirtualFile,
  type GeneratedViteConfig,
} from "../generatedViteConfig.js";
import {
  makeVirtualFilesResolver,
  type VirtualFiles,
} from "../virtualFilesResolver.js";

/** Serves the entry points that Wasp generates from the app. */
export function virtualWaspModules(
  generatedConfig: GeneratedViteConfig,
  generatedViteDir: string,
): Plugin {
  const resolveVirtualFiles = makeVirtualFilesResolver(
    [
      generatedConfig.clientEntryPointPath,
      generatedConfig.routesEntryPointPath,
      generatedConfig.ssrEntryPointPath,
    ].map((id) => ({
      id,
      load: () => readGeneratedVirtualFile(generatedViteDir, id),
    })),
  );
  let virtualFiles!: VirtualFiles;

  return {
    name: "wasp:virtual-wasp-modules",
    enforce: "pre",
    configResolved(config) {
      virtualFiles = resolveVirtualFiles(config.root);
    },
    resolveId: (id) => virtualFiles.ids.get(id),
    load(id) {
      const loader = virtualFiles.loaders.get(id);
      return loader?.();
    },
  };
}
