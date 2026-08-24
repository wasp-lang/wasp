{{={= =}=}}
import { type EnvironmentModuleNode, type Plugin, type ViteDevServer } from "vite";
import {
  clientRuntimeBindingsFilePath,
  getClientEntryTsxContent,
  getClientRuntimeBindingsTsContent,
  getRoutesTsxContent,
  getSsrEntryTsxContent,
} from "../virtual-files/index.js";
import { makeVirtualFilesResolver, type VirtualFiles } from "../virtual-files/resolver.js";
import { validateClientEnv } from "./validateEnv.js";

const resolveVirtualFiles = makeVirtualFilesResolver([
  { id: "{= clientEntryPointPath =}", load: getClientEntryTsxContent },
  { id: "{= clientRuntimeBindingsEntryPointPath =}", load: getClientRuntimeBindingsTsContent },
  { id: "{= routesEntryPointPath =}", load: getRoutesTsxContent },
  { id: "{= ssrEntryPointPath =}", load: getSsrEntryTsxContent },
]);

export function virtualWaspModules(): Plugin {
  let virtualFiles!: VirtualFiles;
  let devServer: ViteDevServer | undefined;
  let restartTimer: ReturnType<typeof setTimeout> | undefined;
  let lastUpdateKey: string | undefined;
  let lastUpdatePromise: Promise<void> | undefined;

  return {
    name: "wasp:virtual-wasp-modules",
    enforce: "pre",
    configResolved(config) {
      virtualFiles = resolveVirtualFiles(config.root);
    },
    configureServer(server) {
      devServer = server;
      server.watcher.add(clientRuntimeBindingsFilePath);
    },
    resolveId: (id) => virtualFiles.ids.get(id),
    load(id) {
      const loader = virtualFiles.loaders.get(id);
      return loader?.();
    },
    async hotUpdate({ file, modules, timestamp }) {
      if (!devServer || !shouldRestartClient(file, modules, virtualFiles)) {
        return;
      }

      const updateKey = `${timestamp}:${file}`;
      if (lastUpdateKey !== updateKey) {
        lastUpdateKey = updateKey;
        lastUpdatePromise = validateClientEnv(devServer.config).then(() => {
          scheduleRestart();
        });
      }

      await lastUpdatePromise;
      return [];
    },
  };

  function scheduleRestart(): void {
    // Restart after Vite finishes the current HMR transaction. Restarting from
    // inside this hook replaces the environments that Vite is still iterating.
    restartTimer ??= setTimeout(() => {
      void devServer
        ?.restart()
        .catch((error) => {
          devServer?.config.logger.error(error);
        })
        .finally(() => {
          restartTimer = undefined;
        });
    });
  }
}

function shouldRestartClient(
  changedFile: string,
  changedModules: EnvironmentModuleNode[],
  virtualFiles: VirtualFiles,
): boolean {
  if (changedFile === clientRuntimeBindingsFilePath) {
    return true;
  }

  const bindingsModuleId = virtualFiles.ids.get("{= clientRuntimeBindingsEntryPointPath =}");
  return bindingsModuleId !== undefined && changedModules.some(
    (module) => hasImporter(module, bindingsModuleId, new Set()),
  );
}

function hasImporter(
  module: EnvironmentModuleNode,
  importerId: string,
  visited: Set<EnvironmentModuleNode>,
): boolean {
  if (module.id === importerId) {
    return true;
  }
  if (visited.has(module)) {
    return false;
  }

  visited.add(module);
  return [...module.importers].some((importer) =>
    hasImporter(importer, importerId, visited),
  );
}
