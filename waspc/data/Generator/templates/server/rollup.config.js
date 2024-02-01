import esbuild from 'rollup-plugin-esbuild'

/** @type {import('rollup')['RollupOptions']} */
export default [
  {
    input: 'src/server.ts',
    plugins: [
      esbuild({
        target: 'esnext',
      }),
    ],
    output: [
      {
        file: `bundle/server.js`,
        format: 'es',
        sourcemap: true,
      },
    ],
    external: (id) => !/^[./]/.test(id),
  },
  {
    input: 'src/dbSeed.ts',
    plugins: [
      esbuild({
        target: 'esnext',
      }),
    ],
    output: [
      {
        file: `bundle/dbSeed.js`,
        format: 'es',
        sourcemap: true,
      },
    ],
    external: (id) => !/^[./]/.test(id),
  },
]
