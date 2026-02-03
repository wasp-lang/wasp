import * as fs from "node:fs/promises"
import * as path from "node:path"
import { createHash } from "node:crypto"

const base = path.resolve(import.meta.dirname, "..")
const cacheFile = path.join(base, ".copy-assets-cache.json")

await copyAssets([
  "./auth/forms/**/*.css",
  "./client/app/components/**/*.css",
  "./client/vite/virtual-files/files/**/*.*",
])

async function copyAssets(globs) {
  try {
    const cache = await loadCache()
    const sourceFiles = await findSourceFiles(globs)

    const { copied, skipped } = await copyChangedFiles(sourceFiles, cache)
    const removed = await removeOrphanedFiles(sourceFiles, cache)
    await saveCache(cache)

    logResults({ copied, skipped, removed })
  } catch (error) {
    console.error("[copy-assets] Fatal error:", error.message)
    process.exit(1)
  }
}

async function copyChangedFiles(sourceFiles, cache) {
  let copied = 0
  let skipped = 0

  for (const file of sourceFiles) {
    try {
      const checksum = await computeChecksum(getSourcePath(file))

      if (await shouldCopyFile(file, checksum, cache)) {
        await copyFile(file)
        copied++
      } else {
        skipped++
      }

      cache.set(file, checksum)
    } catch (error) {
      console.error(`[copy-assets] Error processing ${file}:`, error.message)
      throw error
    }
  }

  return { copied, skipped }
}

async function shouldCopyFile(file, checksum, cache) {
  const cachedChecksum = cache.get(file)

  if (cachedChecksum !== checksum) {
    return true
  }

  return !(await destinationExists(file))
}

async function destinationExists(file) {
  try {
    await fs.access(getDestinationPath(file))
    return true
  } catch {
    return false
  }
}

async function copyFile(file) {
  const dest = getDestinationPath(file)
  await fs.mkdir(path.dirname(dest), { recursive: true })
  await fs.copyFile(getSourcePath(file), dest)
}

async function removeOrphanedFiles(sourceFiles, cache) {
  const sourceSet = new Set(sourceFiles)
  let removed = 0

  for (const [file] of cache.entries()) {
    if (!sourceSet.has(file)) {
      await removeFile(file)
      cache.delete(file)
      removed++
    }
  }

  return removed
}

async function removeFile(file) {
  try {
    await fs.unlink(getDestinationPath(file))
  } catch {
    // File already removed or doesn't exist
  }
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
  return files.sort()
}

async function computeChecksum(filePath) {
  const content = await fs.readFile(filePath)
  return createHash("sha256").update(content).digest("hex")
}

async function saveCache(cache) {
  const cacheObject = Object.fromEntries(cache)
  await fs.writeFile(cacheFile, JSON.stringify(cacheObject, null, 2))
}

async function loadCache() {
  try {
    const content = await fs.readFile(cacheFile, "utf-8")
    const cacheObject = JSON.parse(content)
    return new Map(Object.entries(cacheObject))
  } catch {
    return new Map()
  }
}

function logResults(stats) {
  console.log(
    `[copy-assets] ${stats.copied} copied, ${stats.skipped} skipped, ${stats.removed} removed`,
  )
}
