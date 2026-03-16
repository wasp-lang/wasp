import * as fs from "node:fs/promises"
import * as path from "node:path"

const base = path.resolve(import.meta.dirname, "..")

console.log(`[copy-assets] Starting. Base directory: ${base}`)

await copyAssets([
  "./auth/forms/**/*.css",
  "./client/app/components/**/*.css",
  "./client/vite/virtual-files/files/**/*.*",
])

console.log(`[copy-assets] Done.`)

async function copyAssets(globs) {
  console.log(`[copy-assets] Globbing for patterns:`, globs)
  const sourceFiles = await Array.fromAsync(fs.glob(globs, { cwd: base }))
  console.log(`[copy-assets] Found ${sourceFiles.length} source files:`, sourceFiles)
  const { copied, skipped } = await copyChangedFiles(sourceFiles)
  console.log(`[copy-assets] ${copied} copied, ${skipped} skipped`)
}

async function copyChangedFiles(sourceFiles) {
  let copied = 0
  let skipped = 0

  for (const file of sourceFiles) {
    const sourcePath = getSourcePath(file)
    const destPath = getDestinationPath(file)
    try {
      console.log(`[copy-assets] Checking file: ${file}`)
      if (await shouldCopyFile(sourcePath, destPath)) {
        console.log(`[copy-assets] Copying: ${sourcePath} -> ${destPath}`)
        await copyFile(sourcePath, destPath)
        copied++
        console.log(`[copy-assets] Copied: ${file}`)
      } else {
        console.log(`[copy-assets] Skipping (unchanged): ${file}`)
        skipped++
      }
    } catch (error) {
      console.error(`[copy-assets] Error processing ${file}:`, error.message)
      throw error
    }
  }

  return { copied, skipped }
}

/**
 * File should be copied if:
 * - It doesn't exist in the destination directory.
 * - Its modification time is newer than the destination file.
 *
 * We want to avoid needless copying of files to avoid triggering
 * Vite's file watcher which can cause full page reloads when not
 * necessary.
 */
async function shouldCopyFile(sourcePath, destPath) {
  try {
    console.log(`[copy-assets] Stat-ing source: ${sourcePath}`)
    console.log(`[copy-assets] Stat-ing dest: ${destPath}`)
    const [sourceStat, destStat] = await Promise.all([
      fs.stat(sourcePath),
      fs.stat(destPath),
    ])
    const shouldCopy = sourceStat.mtimeMs > destStat.mtimeMs
    console.log(`[copy-assets] Source mtime: ${sourceStat.mtimeMs}, Dest mtime: ${destStat.mtimeMs}, shouldCopy: ${shouldCopy}`)
    return shouldCopy
  } catch (err) {
    console.log(`[copy-assets] Stat failed (dest likely missing): ${err.message}`)
    // Most likely the destination doesn't exist, so we should copy the file.
    return true
  }
}

async function copyFile(sourcePath, destPath) {
  console.log(`[copy-assets] mkdir: ${path.dirname(destPath)}`)
  await fs.mkdir(path.dirname(destPath), { recursive: true })
  console.log(`[copy-assets] fs.copyFile: ${sourcePath} -> ${destPath}`)
  await fs.copyFile(sourcePath, destPath)
}

function getSourcePath(file) {
  return path.join(base, file)
}

function getDestinationPath(file) {
  return path.join(base, "dist", file)
}
