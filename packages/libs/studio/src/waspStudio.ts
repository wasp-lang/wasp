import { watch } from "node:fs";
import { readFile } from "node:fs/promises";
import { resolve } from "node:path";

import { createPluginFromDevframe } from "@vitejs/devtools-kit/node";
import { defineDevframe } from "devframe";
import type { Plugin } from "vite";

import pkg from "../package.json" with { type: "json" };

export const STUDIO_ID = "wasp-studio";

export interface WaspStudioOptions {
  /**
   * The JSON file describing the app, which Wasp writes on every compile.
   * A relative path resolves from the project root.
   */
  dataFile: string;
}

export interface StudioState {
  /** The parsed data file, or `null` while it doesn't exist yet. */
  data: unknown;
}

/**
 * Adds Wasp Studio as a panel to Vite DevTools. The panel only shows up when
 * the project enables Vite DevTools (`devtools` in its Vite config).
 */
export function waspStudio(options: WaspStudioOptions): Plugin {
  return {
    ...createPluginFromDevframe(createWaspStudioDevframe(options), {
      // Listed first, with the framework's own tools.
      dock: { category: "framework" },
    }),
    apply: "serve",
  };
}

function createWaspStudioDevframe({ dataFile }: WaspStudioOptions) {
  return defineDevframe({
    id: STUDIO_ID,
    name: "Wasp Studio",
    version: pkg.version,
    packageName: pkg.name,
    importMetaUrl: import.meta.url,
    homepage: pkg.homepage,
    description: pkg.description,
    icon: "ph:graph-duotone",
    clientAssets: resolve(import.meta.dirname, "client"),
    async setup(ctx) {
      const studio = ctx.scope(STUDIO_ID);
      const dataFilePath = resolve(ctx.cwd, dataFile);

      const state = await studio.rpc.sharedState<StudioState>("state", {
        initialValue: { data: await readDataFile(dataFilePath) },
      });

      // Wasp rewrites the file on every compile, which updates the open panels.
      watch(dataFilePath, async () => {
        const data = await readDataFile(dataFilePath);
        if (data !== null) {
          state.mutate((draft) => {
            draft.data = data;
          });
        }
      });
    },
  });
}

async function readDataFile(path: string): Promise<unknown> {
  try {
    return JSON.parse(await readFile(path, "utf8"));
  } catch {
    // Missing, or caught mid-write. The next change event retries.
    return null;
  }
}
