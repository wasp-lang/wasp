{{={= =}=}}
import esbuild from 'rollup-plugin-esbuild'
import alias from '@rollup/plugin-alias';
import path from 'path'
import resolve from '@rollup/plugin-node-resolve';

export default [
  createBundle('src/server.ts', 'bundle/server.js'),
  {=# areDbSeedsDefined =}
  createBundle('src/dbSeed.ts', 'bundle/dbSeed.js'),
  {=/ areDbSeedsDefined =}
]
// Utility function to read and parse tsconfig.json
function getAliasEntries() {
  const paths = {
    {=# paths =}
      "{= key =}": [{=# value =}"{= . =}"{=/ value =}],
    {=/ paths =}
  }
  const baseUrl = "{= baseUrl =}"
  return Object.entries(paths).map(([alias, [relativePath]]) => ({
    find: alias.replace('/*', ''),
    replacement: path.join("../", baseUrl, relativePath.replace('/*', '')),
  }));
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
      alias({
        entries: getAliasEntries(),
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

/*
import esbuild from 'rollup-plugin-esbuild'
import alias from '@rollup/plugin-alias';
import resolve from '@rollup/plugin-node-resolve';

export default [
  createBundle('src/server.ts', 'bundle/server.js'),
  createBundle('src/dbSeed.ts', 'bundle/dbSeed.js'),
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
      alias({
        entries: [
          { find: '@util', replacement: './util.js' },
        ]
      }),
      esbuild(),
    ],

    // // We don't want to bundle any of the node_module deps
    // // as we want to keep them as external dependencies
    // external: (id) => !/^[./]/.test(id),
    external: /node_modules/,
  }
}
*/
