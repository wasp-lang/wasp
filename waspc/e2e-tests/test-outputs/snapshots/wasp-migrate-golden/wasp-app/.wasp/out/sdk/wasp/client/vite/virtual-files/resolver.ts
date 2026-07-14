import * as path from "node:path"

export type VirtualFiles = {
  ids: ReadonlyMap<string, string>;
  loaders: ReadonlyMap<string, () => Promise<string> | string>;
};

type VirtualFilesDefinition = {
  id: string;
  load: () => Promise<string> | string;
};

export const makeVirtualFilesResolver =
  (files: VirtualFilesDefinition[]) =>
  (rootPath: string): VirtualFiles => {
    const filesWithAbsPath = files.map((d) => ({
      ...d,
      absPath: path.resolve(rootPath, path.basename(d.id)),
    }));

    const ids = new Map(
      filesWithAbsPath.flatMap((d) =>
        // We'll resolve both the relative and absolute paths for the virtual
        // files, since Vite and other plugins may request either.
        [
          [d.id, d.absPath],
          [d.absPath, d.absPath],
        ],
      ),
    );

    const loaders = new Map(filesWithAbsPath.map((d) => [d.absPath, d.load]));

    return { ids, loaders };
  };
