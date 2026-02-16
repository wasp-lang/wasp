import * as path from "node:path";

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

    const ids = new Map(filesWithAbsPath.map((d) => [d.id, d.absPath]));

    const loaders = new Map(
      filesWithAbsPath.map((d) => [d.absPath, d.load]),
    );

    return { ids, loaders };
  };
