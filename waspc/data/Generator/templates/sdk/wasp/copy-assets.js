import * as fs from "node:fs/promises";
import * as path from "node:path";

const base = path.resolve(import.meta.dirname);

await copyAssets([
  "./user-core/auth/forms/**/*.css",
  "./user-core/client/app/components/**/*.css",
  "./user-core/client/vite/virtual-files/files/**/*.*",
])

async function copyAssets(globs) {
  const sourceFiles = await Array.fromAsync(fs.glob(globs, { cwd: base }))
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
      if (await shouldCopyFile(sourcePath, destPath)) {
        await copyFile(sourcePath, destPath)
        copied++
      } else {
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
    const [sourceStat, destStat] = await Promise.all([
      fs.stat(sourcePath),
      fs.stat(destPath),
    ])
    return sourceStat.mtimeMs > destStat.mtimeMs
  } catch {
    // Most likely the destination doesn't exist, so we should copy the file.
    return true
  }
}

async function copyFile(sourcePath, destPath) {
  await fs.mkdir(path.dirname(destPath), { recursive: true })
  await fs.copyFile(sourcePath, destPath)
}

function getSourcePath(file) {
  return path.join(base, file)
}

function getDestinationPath(file) {
  return path.join(base, "dist", file)
}
