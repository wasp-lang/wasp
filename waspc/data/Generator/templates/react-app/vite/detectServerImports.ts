{{={= =}=}}
import { type Plugin } from 'vite'
import path from 'path'

const waspProjectDirAbsPath = getWaspProjectDirAbsPathFromCwd()

export function detectServerImports(): Plugin {
  return {
    name: 'wasp-detect-server-imports',
    enforce: 'pre',
    resolveId(source, importer) {
      if (!importer) {
        return
      }

      const importerRelativePath = getPathRelativeToWaspProjectDir(importer)

      if (!isPathToUserCode(importerRelativePath)) {
        return
      }

      ensureNoServerImports(source, importerRelativePath)
    },
  }
}

type ImportCheckPredicate = (moduleName: string) => boolean

const serverImportChecks: ImportCheckPredicate[] = [
  (moduleName: string) => moduleName.startsWith('wasp/server'),
]

function ensureNoServerImports(source: string, relativeImporter: RelativePath): void {
  for (const check of serverImportChecks) {
    if (check(source)) {
      throw new Error(
        `Server code cannot be imported in the client code. Import from "${source}" in "${relativeImporter.relativePath}" is not allowed.`
      )
    }
  }
}

type RelativePath = {
  relativePath: string
}

function getPathRelativeToWaspProjectDir(filePath: string): RelativePath {
  return { relativePath: path.relative(waspProjectDirAbsPath, filePath) }
}

function isPathToUserCode(filePath: RelativePath): boolean {
  return filePath.relativePath.startsWith('{= srcDirInWaspProjectDir =}')
}

// We can't pass the "waspProjectDir" path from Haskell because we need the absolute path:
// e.g. /Users/{username}/dev/wasp/waspc/examples/todoApp
// which contains machine specific info like the username which is different in the CI and locally.
// This breaks our e2e tests in the CI because the path is different.
function getWaspProjectDirAbsPathFromCwd(): string {
  const webAppDirAbsPath = process.cwd()
  const waspProjectDirAbsPath = path.join(webAppDirAbsPath, '{= waspProjectDirFromWebAppDir =}')
  return waspProjectDirAbsPath
}
