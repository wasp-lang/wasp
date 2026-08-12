import * as path from "node:path"

export type VirtualFiles = {
  resolveId: (id: string) => string | undefined;
  load: (id: string) => Promise<string> | string | undefined;
};

type VirtualFilesDefinition = {
  id: string;
  load: () => Promise<string> | string;
};

/**
 * Virtual files pretend to be files sitting in the project's root directory,
 * even though nothing is ever written to disk. This computes the path they
 * pretend to be at, which is also the ID Vite and Rollup use for them
 * internally.
 */
export function getVirtualFileAbsPath(rootPath: string, id: string): string {
  return path.resolve(rootPath, path.basename(id));
}

export const makeVirtualFilesResolver =
  (files: VirtualFilesDefinition[]) =>
  (rootPath: string): VirtualFiles => {
    const filesWithAbsPath = files.map((d) => ({
      ...d,
      absPath: getVirtualFileAbsPath(rootPath, d.id),
    }));

    const absPathsById = new Map(
      filesWithAbsPath.flatMap((d) =>
        // We'll resolve all the spellings of a virtual file's path, since Vite
        // and other plugins may request any of them: our own ID, the absolute
        // path, and the path relative to the project root (which is how Vite
        // refers to files it serves in dev).
        [
          [d.id, d.absPath],
          [d.absPath, d.absPath],
          ["/" + path.basename(d.id), d.absPath],
        ],
      ),
    );

    const loaders = new Map(filesWithAbsPath.map((d) => [d.absPath, d.load]));

    return {
      resolveId(id) {
        // Plugins can ask for a variant of a file by appending a query to its
        // path (e.g. Nitro's `?assets=client`). We resolve those too, keeping
        // the query, so the plugin that added it still recognizes the result.
        const [pathPart, query] = splitOffQuery(id);
        const absPath = absPathsById.get(pathPart);
        return absPath === undefined ? undefined : absPath + query;
      },
      load(id) {
        // Only the plain file is ours to load. IDs with a query belong to the
        // plugin that introduced the query.
        return loaders.get(id)?.();
      },
    };
  };

function splitOffQuery(id: string): [pathPart: string, query: string] {
  const queryStart = id.indexOf("?");
  return queryStart === -1
    ? [id, ""]
    : [id.slice(0, queryStart), id.slice(queryStart)];
}
