import esbuild from 'rollup-plugin-esbuild'
import resolve from '@rollup/plugin-node-resolve';
import { resolve as resolvePath } from 'node:path'
import {
  discoverWaspModulePackages,
  shouldExternalize,
} from './rollupPackages.js'

const appRootDir = resolvePath(import.meta.dirname, '../../..')
const packagesToBundle = new Set([
  'wasp',
  ...discoverWaspModulePackages(appRootDir),
])

export default [
  createBundle('src/server.ts', 'bundle/server.js'),
]

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
      esbuild({
        target: 'esnext',
      }),
    ],
    // Wasp modules must share the host's bundled Wasp runtime. Their own
    // third-party dependencies remain external.
    external: (id) => shouldExternalize(id, packagesToBundle),
    // 'preserveSymlinks: false' tells Rollup to fully follow symlinks when
    // resolving modules. This is the default option, but we're setting it
    // explicitly because we rely on it.
    //
    // With this set to 'false', the 'wasp' package resolves to
    // '.wasp/out/sdk/wasp' (not 'node_modules/wasp'), preventing Rollup from
    // viewing it as an external dependency and ensuring it gets bundled.
    //
    // We need to bundle 'wasp' to support fully extensionless relative
    // imports. See https://github.com/wasp-lang/wasp/issues/2492 for more
    // details.
    //
    // Source: https://rollupjs.org/configuration-options/#preservesymlinks
    preserveSymlinks: false,
  }
}
