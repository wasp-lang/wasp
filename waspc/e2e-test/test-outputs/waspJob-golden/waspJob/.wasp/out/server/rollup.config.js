import esbuild from 'rollup-plugin-esbuild'
import resolve from '@rollup/plugin-node-resolve';

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
    // We don't want to bundle any of the node_module deps because we want to
    // keep them as external dependencies.
    external: /node_modules/,
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
