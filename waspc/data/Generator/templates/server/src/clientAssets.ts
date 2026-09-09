{{={= =}=}}
import path from "node:path";
import { existsSync } from "node:fs";
import express from "express";
import type { Express,  NextFunction,  Request,  Response } from "express";
import type { Config } from "wasp/server";

// The client is built into the generated web-app dir, next to the server bundle.
const clientBuildDir = path.resolve(import.meta.dirname, "{=& clientBuildDirFromServerBundleDir =}");
const assetsDirInClientBuildDir = "{= assetsDir =}";

/**
 * Serves the built client as static assets from the server.
 */
export function serveClientAssets(app: Express, config: Config): void {
  // "" when the client lives at the root, otherwise e.g. "/my-app".
  const clientBaseDir = config.clientBaseDir
  const clientMountPath = clientBaseDir === "" ? "/" : clientBaseDir

  app.use(
    clientMountPath,
    express.static(clientBuildDir, {
      index: false,
      redirect: false,
      setHeaders: setCacheHeaders,
    })
  )

  if (clientBaseDir !== "") {
    app.get("/", (_req, res) => {
      res.redirect(clientBaseDir)
    })
  }

  app.use(clientMountPath, serveClientPage)
}

function setCacheHeaders(res: Response, filePath: string): void {
  if (isFileInAssetsDir(filePath)) {
    // Vite's assets can be cached forever.
    // See `isFileInAssetsDir` for more details.
    res.setHeader("Cache-Control", "public, max-age=31536000, immutable")
  } else if (filePath.endsWith(".html")) {
    // Always revalidated HTML files.
    res.setHeader("Cache-Control", "no-cache")
  }
}

/**
 * Whether the file lives in the Vite's assets dir.
 * 
 * Vite suffixes every file it puts there with the file's hash,
 * so any change to data creates a new file name (and new URL),
 * which is why they are safe to cache forever.
 *
 * TODO: See if `public/assets/` dir would pose any trouble for users.
 *       If yes, agree on how to deal with it.
 */
function isFileInAssetsDir(filePath: string): boolean {
  const filePathInBuildDir = path.relative(clientBuildDir, filePath)
  return filePathInBuildDir.startsWith(`${assetsDirInClientBuildDir}${path.sep}`)
}

/**
 * Serves the page for a client route.
 * Serves the prerendered `index.html` if there is one, otherwise the SPA fallback page.
 */
function serveClientPage(req: Request, res: Response, next: NextFunction): void {
  if (req.method !== "GET" && req.method !== "HEAD") {
    return next()
  }
  if (isServerRequest(req)) {
    return next()
  }
  // A missing file under the assets dir is a broken link,
  // so it gets a 404 instead of the SPA fallback page.
  if (isInAssetsDir(req.path)) {
    return next()
  }

  const pageFile = findPrerenderedPageFile(req.path) ?? spaFallbackPageFile;
  setCacheHeaders(res, pageFile)
  // `root` is required. Without it `sendFile` applies its dotfile check to the whole
  // absolute path and rejects the `.wasp` directory the build lives in.
  const sendFileOptions = { root: clientBuildDir, cacheControl: false };
  res.sendFile(path.relative(clientBuildDir, pageFile), sendFileOptions, (err) => {
    if (err) next(err)
  })
}
const spaFallbackPageFile = path.join(clientBuildDir, "200.html");

/**
 * Whether the request should be handled by the server.
 */
function isServerRequest(req: Request): boolean {
  const pathname = req.originalUrl.split('?')[0];
  // Express routes are case insensitive.
  const normalizedRequestPath = pathname.toLowerCase();
  return (
    serverGetExactPaths.some(
      (path) => stripTrailingSlashes(path.toLowerCase()) === stripTrailingSlashes(normalizedRequestPath)
    ) ||
    serverGetSubtreePaths.some((path) => isPathSegmentPrefixOf(path.toLowerCase(), normalizedRequestPath))
  )
}
// Only the paths the server answers `GET` on: this decides between the server and a page,
// and a page is only ever asked for with `GET` (or `HEAD`, which Express serves with `GET`).
const serverGetExactPaths: string[] = {=& serverGetExactPaths =}
const serverGetSubtreePaths: string[] = {=& serverGetSubtreePaths =}

function stripTrailingSlashes(path: string): string {
  const stripped = path.replace(/\/+$/, "")
  return stripped === "" ? "/" : stripped
}

function isPathSegmentPrefixOf(prefix: string, pathname: string): boolean {
  return pathname === prefix || pathname.startsWith(`${prefix}/`)
}

function isInAssetsDir(requestPath: string): boolean {
  return requestPath.startsWith(`/${assetsDirInClientBuildDir}/`)
}

function findPrerenderedPageFile(requestPath: string): string | undefined {
  const pageFile = path.join(clientBuildDir, decodePath(requestPath), "index.html")
  const isInsideBuildDir = pageFile.startsWith(`${clientBuildDir}${path.sep}`)
  if (isInsideBuildDir && existsSync(pageFile)) {
    return pageFile
  }
  return undefined
}

function decodePath(requestPath: string): string {
  try {
    return decodeURIComponent(requestPath)
  } catch {
    return requestPath
  }
}
