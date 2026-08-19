{{={= =}=}}
import { type Plugin } from 'vite'
import path from 'path'
import { ENVIRONMENT_NAMES } from '../../../vite/constants.js'

export function detectServerImports(): Plugin {
  let parsePathToUserCode!: ParsePathToUserCodeFn
  return {
    name: 'wasp:detect-server-imports',
    enforce: 'pre',
    // Importing server code is only forbidden in the environments that process
    // client code.
    applyToEnvironment: (environment) =>
      environment.name === ENVIRONMENT_NAMES.CLIENT ||
      environment.name === ENVIRONMENT_NAMES.SSR,
    configResolved(config) {
      parsePathToUserCode = createPathToUserCodeParser(config.root)
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
    return importerPathRelativeToWaspProjectDir.startsWith('{= srcDirInWaspProjectDir =}')
      ? (importerPathRelativeToWaspProjectDir as RelativePathToUserCode)
      : null
  }
}
