{{={= =}=}}
import esbuild from 'rollup-plugin-esbuild'
import resolve from '@rollup/plugin-node-resolve';

const modulePackageNames = [
  {=# modulePackageNames =}
  "{= name =}",
  {=/ modulePackageNames =}
];

export default [
  createBundle('src/server.ts', 'bundle/server.js'),
  {=# areDbSeedsDefined =}
  createBundle('src/dbSeed.ts', 'bundle/dbSeed.js'),
  {=/ areDbSeedsDefined =}
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
    //
    // We also externalize module packages (e.g. @waspello/todo-module) because
    // they may be symlinked via file: dependencies, causing their resolved
    // paths to fall outside node_modules. Bundling them would break
    // import.meta.url-based package name resolution at runtime.
    external: (id) => {
      if (/node_modules/.test(id)) return true;
      for (const pkg of modulePackageNames) {
        if (id === pkg || id.startsWith(pkg + '/')) return true;
      }
      return false;
    },
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
