import * as path from "node:path";
export const makeVirtualFilesResolver = (files) => (rootPath) => {
    const filesWithAbsPath = files.map((d) => ({
        ...d,
        absPath: path.resolve(rootPath, path.basename(d.id)),
    }));
    const ids = new Map(filesWithAbsPath.flatMap((d) => 
    // We'll resolve both the relative and absolute paths for the virtual
    // files, since Vite and other plugins may request either.
    [
        [d.id, d.absPath],
        [d.absPath, d.absPath],
    ]));
    const loaders = new Map(filesWithAbsPath.map((d) => [d.absPath, d.load]));
    return { ids, loaders };
};
//# sourceMappingURL=resolver.js.map