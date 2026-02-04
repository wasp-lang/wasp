import * as fs from "node:fs/promises"
import * as path from "node:path"

const base = path.resolve(import.meta.dirname, "..")

await copyAssets([
  "./auth/forms/**/*.css",
  "./client/app/components/**/*.css",
  "./client/vite/virtual-files/files/**/*.*",
])

async function copyAssets(globs) {
  try {
    const sourceFiles = await findSourceFiles(globs)
    const { copied, skipped } = await copyChangedFiles(sourceFiles)
    console.log(`[copy-assets] ${copied} copied, ${skipped} skipped`)
  } catch (error) {
    console.error("[copy-assets] Fatal error:", error.message)
    process.exit(1)
  }
}

async function copyChangedFiles(sourceFiles) {
  let copied = 0
  let skipped = 0

  for (const file of sourceFiles) {
    try {
      if (await shouldCopyFile(file)) {
        await copyFile(file)
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
 */
async function shouldCopyFile(file) {
  const sourcePath = getSourcePath(file)
  const destPath = getDestinationPath(file)

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

async function copyFile(file) {
  const dest = getDestinationPath(file)
  await fs.mkdir(path.dirname(dest), { recursive: true })
  await fs.copyFile(getSourcePath(file), dest)
}

function getSourcePath(file) {
  return path.join(base, file)
}

function getDestinationPath(file) {
  return path.join(base, "dist", file)
}

async function findSourceFiles(globs) {
  const files = []
  for await (const file of fs.glob(globs, { cwd: base })) {
    files.push(file)
  }
  return files
}
