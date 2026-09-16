export type VirtualFiles = {
    ids: ReadonlyMap<string, string>;
    loaders: ReadonlyMap<string, () => Promise<string> | string>;
};
type VirtualFilesDefinition = {
    id: string;
    load: () => Promise<string> | string;
};
export declare const makeVirtualFilesResolver: (files: VirtualFilesDefinition[]) => (rootPath: string) => VirtualFiles;
export {};
//# sourceMappingURL=resolver.d.ts.map