import * as fs from "node:fs/promises"
import * as path from "node:path"
import { defineConfig } from "tsdown"

// Rolldown plugin to externalize non-JS asset imports (CSS, SVG, images, etc.)
// These will be processed by Vite when the web-app bundles the SDK.
const assetExtensions = /\.(css|svg|png|jpe?g|gif|webp|ico|woff2?|ttf|eot)$/

function externalizeAssets() {
  return {
    name: "externalize-assets",
    resolveId(id: string) {
      if (assetExtensions.test(id)) {
        return { id, external: true }
      }
    },
  }
}

export default defineConfig({
  entry: [
    "./**/*.ts",
    "./**/*.tsx",
    "!./node_modules/**",
    "!./dist/**",
    "!./client/vite/virtual-files/files/**",
  ],
  outDir: "dist",
  format: "esm",
  unbundle: true,
  sourcemap: true,
  skipNodeModulesBundle: true,
  // The package already has "type": "module", so .js is ESM.
  // tsdown defaults to .mjs which doesn't match the exports map.
  outExtensions() {
    return { js: ".js", dts: ".d.ts" }
  },
  tsconfig: "tsconfig.build.json",
  dts: {
    sourcemap: true,
  },
  plugins: [externalizeAssets()],
  hooks: {
    "build:done": async () => {
      const base = import.meta.dirname
      const globs = [
        // CSS files must be in dist/ so Vite can resolve the imports.
        "./auth/forms/**/*.css",
        "./client/app/components/**/*.css",
        // Virtual-files are Mustache templates read at runtime by the Vite plugin.
        // They can't be compiled (they have dynamic imports that don't exist at build time).
        "./client/vite/virtual-files/files/**/*.*",
      ]
      const sourceFiles = await Array.fromAsync(fs.glob(globs, { cwd: base }))
      for (const file of sourceFiles) {
        const src = path.join(base, file)
        const dest = path.join(base, "dist", file)
        await fs.mkdir(path.dirname(dest), { recursive: true })
        await fs.copyFile(src, dest)
      }
    },
  },
})
