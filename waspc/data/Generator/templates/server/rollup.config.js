import esbuild from 'rollup-plugin-esbuild'

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
      esbuild({
        target: 'esnext',
      }),
    ],
    // We don't want to bundle any of the node_module deps
    // as we want to keep them as external dependencies
    external: (id) => !/^[./]/.test(id),
  }
}
