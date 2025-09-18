import { type Plugin } from 'vite'
import path from 'path'
import { resolveProjectPath } from 'wasp/dev'

export function detectServerImports(): Plugin {
  return {
    name: 'wasp-detect-server-imports',
    enforce: 'pre',
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

function parsePathToUserCode(
  importerPath: string
): RelativePathToUserCode | null {
  const importerPathRelativeToWaspProjectDir = path.relative(
    getWaspProjectDirAbsPathWhileInWebAppDir(),
    importerPath
  )
  return importerPathRelativeToWaspProjectDir.startsWith('src/')
    ? (importerPathRelativeToWaspProjectDir as RelativePathToUserCode)
    : null
}

// We can't pass the "waspProjectDir" path from Haskell because we need the absolute path:
// e.g. /Users/{username}/dev/wasp/waspc/examples/todoApp
// which contains machine specific info like the username which is different in the CI and locally.
// This breaks our e2e tests in the CI because the path is different.
function getWaspProjectDirAbsPathWhileInWebAppDir(): string {
  return path.resolve(resolveProjectPath('./'))
}
