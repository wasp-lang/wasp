{{={= =}=}}
import esbuild from 'rollup-plugin-esbuild'
import resolve from '@rollup/plugin-node-resolve';
import { resolveProjectPath } from "wasp/dev"
import path from 'path'

export default [
  createBundle('src/server.ts', 'bundle/server.js'),
  {=# areDbSeedsDefined =}
  createBundle('src/dbSeed.ts', 'bundle/dbSeed.js'),
  {=/ areDbSeedsDefined =}
]

function resolvePathAliases(aliasesToSrcPaths = {}) {
  return {
    name: 'path-alias-resolver',
    async resolveId(importeeString, importerAbsPath, resolveOptions) {
      const userDefinedLookupLocation = aliasesToSrcPaths[importeeString]

      if (userDefinedLookupLocation === undefined) {
        return null;
      }

      const absWorkingDir = process.cwd()
      const waspProjectDirAbsPath = path.resolve(absWorkingDir, resolveProjectPath("./"))
      const lookupLocationAbs = path.resolve(waspProjectDirAbsPath, userDefinedLookupLocation)
      const lookupLocationFromImporterPath = "./" + path.relative(path.dirname(importerAbsPath), lookupLocationAbs)

      const result = await this.resolve(lookupLocationFromImporterPath, importerAbsPath, { ...resolveOptions, skipSelf: true })
      // TODO: debugging output, remove
      // console.table({
      //   userDefinedAlias: importeeString,
      //   userDefinedLookupLocation,
      //   importerAbsPath,
      //   lookupLocationFromImporterPath,
      // })
      return result
    }
  };
}

function createBundle(inputFilePath, outputFilePath) {
  return {
    input: inputFilePath,
    output: {
      file: outputFilePath,
      format: 'es',
      sourcemap: true,
    },
    plugins: [
      resolve(),
      resolvePathAliases({
        '@util': './src/util.js',
        '@components': './src/components',
      }),
      esbuild({
        target: 'esnext',
      }),
    ],
    // We don't want to bundle any of the node_module deps
    // as we want to keep them as external dependencies
    external: /node_modules/,
  }
}
