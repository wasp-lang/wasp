import path from 'path';
export function detectServerImports() {
    let parsePathToUserCode;
    return {
        name: 'wasp:detect-server-imports',
        enforce: 'pre',
        configResolved(config) {
            parsePathToUserCode = createPathToUserCodeParser(config.root);
        },
        resolveId(source, importer) {
            if (!importer) {
                return;
            }
            const pathToUserCode = parsePathToUserCode(importer);
            if (!pathToUserCode) {
                return;
            }
            if (isServerImport(source)) {
                throw new Error(`Server code cannot be imported in the client code. Import from "${source}" in "${pathToUserCode}" is not allowed.`);
            }
        },
    };
}
function isServerImport(moduleName) {
    return moduleName.startsWith('wasp/server');
}
function createPathToUserCodeParser(waspProjectDirPath) {
    return (importerPath) => {
        const importerPathRelativeToWaspProjectDir = path.relative(waspProjectDirPath, importerPath);
        return importerPathRelativeToWaspProjectDir.startsWith('src/')
            ? importerPathRelativeToWaspProjectDir
            : null;
    };
}
//# sourceMappingURL=detectServerImports.js.map