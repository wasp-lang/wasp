import { type Plugin } from 'vite'
import path from 'path'

export function detectServerImports(): Plugin {
  return {
    name: 'wasp-detect-server-imports',
    enforce: 'pre',
    resolveId(source, importer) {
      if (!importer) {
        return
      }

      const importerRelativePath = getRelativeFilePath(importer)

      // Check only for imports from src/ directory which
      // contains the user's code.
      if (!importerRelativePath.startsWith('src/')) {
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

function ensureNoServerImports(source: string, relativeImporter: string): void {
  for (const check of serverImportChecks) {
    if (check(source)) {
      throw new Error(
        `Server code cannot be imported in the client code. Import from '${source}' in '${relativeImporter}' is not allowed.`
      )
    }
  }
}

const waspProjectDirAbsPath = getWaspProjectDirAbsPathFromCwd()

function getRelativeFilePath(filePath: string): string {
  return path.relative(waspProjectDirAbsPath, filePath)
}

// We are not passing the waspProjectDir path from Haskell because
// our e2e tests stop working. Becuase we need to absolute path of the
// Wasp project directory, it contains things like the username of the
// user running the tests, which is different on different machines.
function getWaspProjectDirAbsPathFromCwd(): string {
  const webAppDirAbsPath = process.cwd()
  const waspProjectDirAbsPath = path.join(webAppDirAbsPath, '../../../')
  return waspProjectDirAbsPath
}
