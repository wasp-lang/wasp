import { type Plugin } from 'vite'
import path from 'path'

let parsePathToUserCode: ParsePathToUserCodeFn;

export function detectServerImports(): Plugin {
  return {
    name: 'wasp:detect-server-imports',
    enforce: 'pre',
    configResolved(config) {
      parsePathToUserCode = createPathToUserCodeParser(config.root);
    },
    resolveId(source, importer) {
      if (!importer) {
        return
      }

      const pathToUserCode = parsePathToUserCode(importer)
      if (!pathToUserCode) {
        return
      }

      if (isServerImport(source)) {
        throw new Error(
          `Server code cannot be imported in the client code. Import from "${source}" in "${pathToUserCode}" is not allowed.`
        )
      }
    },
  }
}

function isServerImport(moduleName: string): boolean {
  return moduleName.startsWith('wasp/server')
}

type RelativePathToUserCode = string & { _brand: 'relativePathToUserCode' }

type ParsePathToUserCodeFn = (importerPath: string) => RelativePathToUserCode | null;

function createPathToUserCodeParser(waspProjectDirPath: string): ParsePathToUserCodeFn {
  return (importerPath: string): RelativePathToUserCode | null => {
    const importerPathRelativeToWaspProjectDir = path.relative(
      waspProjectDirPath,
      importerPath
    )
    return importerPathRelativeToWaspProjectDir.startsWith('src/')
      ? (importerPathRelativeToWaspProjectDir as RelativePathToUserCode)
      : null
  }
}
