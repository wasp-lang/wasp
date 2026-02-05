import { createReadStream } from 'node:fs'
import fs from 'node:fs/promises'
import http from 'node:http'
import path from 'node:path'
import { fileURLToPath } from 'node:url'

import {
  baseDir,
  isSsrRoute,
  render,
} from './build-ssr/entry-server.js'

const __dirname = path.dirname(fileURLToPath(import.meta.url))
const clientBuildDir = path.join(__dirname, 'build')
const indexHtmlPath = path.join(clientBuildDir, 'index.html')
const indexHtml = await fs.readFile(indexHtmlPath, 'utf-8')

function parseArgs(argv) {
  const args = argv.slice(2)
  let port = Number(process.env.PORT) || 4173
  let strictPort = false

  for (let i = 0; i < args.length; i++) {
    const arg = args[i]
    if (arg === '--port') {
      const portArg = args[i + 1]
      if (portArg !== undefined) {
        const parsedPort = Number(portArg)
        if (Number.isFinite(parsedPort) && Number.isInteger(parsedPort) && parsedPort >= 1 && parsedPort <= 65535) {
          port = parsedPort
          i++
        } else {
          console.error(`Invalid port value: ${portArg}. Using default port ${port}.`)
        }
      } else {
        console.error(`Missing port value after --port. Using default port ${port}.`)
      }
      continue
    }
    if (arg === '--strictPort') {
      strictPort = true
    }
  }

  return { port, strictPort }
}

function stripBase(pathname) {
  if (baseDir === '/' || baseDir === '') {
    return pathname
  }
  const normalizedBase = baseDir.endsWith('/')
    ? baseDir.slice(0, -1)
    : baseDir
  if (pathname === normalizedBase) {
    return '/'
  }
  if (pathname.startsWith(normalizedBase + '/')) {
    return pathname.slice(normalizedBase.length)
  }
  return null
}

function getContentType(filePath) {
  if (filePath.endsWith('.html')) return 'text/html'
  if (filePath.endsWith('.js')) return 'text/javascript'
  if (filePath.endsWith('.mjs')) return 'text/javascript'
  if (filePath.endsWith('.css')) return 'text/css'
  if (filePath.endsWith('.json')) return 'application/json'
  if (filePath.endsWith('.svg')) return 'image/svg+xml'
  if (filePath.endsWith('.png')) return 'image/png'
  if (filePath.endsWith('.jpg') || filePath.endsWith('.jpeg')) return 'image/jpeg'
  if (filePath.endsWith('.gif')) return 'image/gif'
  if (filePath.endsWith('.webp')) return 'image/webp'
  if (filePath.endsWith('.ico')) return 'image/x-icon'
  if (filePath.endsWith('.map')) return 'application/json'
  if (filePath.endsWith('.woff')) return 'font/woff'
  if (filePath.endsWith('.woff2')) return 'font/woff2'
  if (filePath.endsWith('.ttf')) return 'font/ttf'
  return 'application/octet-stream'
}

async function tryServeStatic(strippedPathname, res) {
  if (strippedPathname === '/' || strippedPathname === '') {
    return false
  }

  const relativePath = strippedPathname.startsWith('/')
    ? strippedPathname.slice(1)
    : strippedPathname

  const resolvedPath = path.join(clientBuildDir, relativePath)
  // Use path.relative to safely check if resolvedPath is inside clientBuildDir
  const relativeFromBuild = path.relative(clientBuildDir, resolvedPath)
  if (relativeFromBuild.startsWith('..') || path.isAbsolute(relativeFromBuild)) {
    return false
  }

  try {
    const stat = await fs.stat(resolvedPath)
    if (!stat.isFile()) {
      return false
    }
  } catch {
    return false
  }

  res.statusCode = 200
  res.setHeader('Content-Type', getContentType(resolvedPath))
  const stream = createReadStream(resolvedPath)
  stream.on('error', (err) => {
    console.error(`Error reading file ${resolvedPath}:`, err)
    if (!res.headersSent) {
      res.statusCode = 500
      res.setHeader('Content-Type', 'text/plain')
    }
    res.end('Internal Server Error')
  })
  stream.pipe(res)
  return true
}

async function handleRequest(req, res) {
  const url = new URL(req.url ?? '/', `http://${req.headers.host}`)
  const strippedPathname = stripBase(url.pathname)

  if (!strippedPathname) {
    res.statusCode = 200
    res.setHeader('Content-Type', 'text/html')
    res.end(indexHtml)
    return
  }

  if (await tryServeStatic(strippedPathname, res)) {
    return
  }

  if (isSsrRoute(url.pathname)) {
    const appHtml = await render(url.pathname + url.search)
    const html = indexHtml
      .replace('<div id="root">', '<div id="root" data-wasp-ssr="1">')
      .replace('<!--ssr-outlet-->', appHtml)

    res.statusCode = 200
    res.setHeader('Content-Type', 'text/html')
    res.end(html)
    return
  }

  res.statusCode = 200
  res.setHeader('Content-Type', 'text/html')
  res.end(indexHtml)
}

const { port, strictPort } = parseArgs(process.argv)
const server = http.createServer((req, res) => {
  handleRequest(req, res).catch((error) => {
    console.error(error)
    res.statusCode = 500
    res.setHeader('Content-Type', 'text/plain')
    res.end('Internal Server Error')
  })
})

server.listen(port)

server.on('error', (error) => {
  if (error.code === 'EADDRINUSE' && strictPort) {
    console.error(`Port ${port} is already in use.`)
    process.exit(1)
  }
  console.error(error)
  process.exit(1)
})

server.on('listening', () => {
  console.log(`SSR server listening on port ${port}`)
})
