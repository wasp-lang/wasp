import { createReadStream } from "node:fs";
import fs from "node:fs/promises";
import http from "node:http";
import path from "node:path";
import { fileURLToPath } from "node:url";

// Provide minimal browser API stubs so that client-side code which accesses
// localStorage, sessionStorage, window, or document at module-init time does
// not crash during SSR. These stubs return safe no-op / empty values.
if (typeof globalThis.window === "undefined") {
  const noopStorage = {
    getItem: () => null,
    setItem: () => {},
    removeItem: () => {},
    clear: () => {},
    key: () => null,
    get length() { return 0; },
  };
  globalThis.localStorage = noopStorage;
  globalThis.sessionStorage = noopStorage;
  // Since window = globalThis, libraries like react-router call
  // window.addEventListener etc. Add these on globalThis itself.
  globalThis.addEventListener = globalThis.addEventListener || (() => {});
  globalThis.removeEventListener = globalThis.removeEventListener || (() => {});
  globalThis.dispatchEvent = globalThis.dispatchEvent || (() => true);
  globalThis.window = globalThis;
  globalThis.document = {
    documentElement: {
      classList: { add: () => {}, remove: () => {}, contains: () => false, toggle: () => false },
      style: {},
      setAttribute: () => {},
      getAttribute: () => null,
    },
    body: {
      classList: { add: () => {}, remove: () => {}, contains: () => false, toggle: () => false },
      style: {},
    },
    createElement: () => ({
      style: {},
      setAttribute: () => {},
      getAttribute: () => null,
      appendChild: () => {},
      classList: { add: () => {}, remove: () => {} },
    }),
    createTextNode: () => ({}),
    querySelector: () => null,
    querySelectorAll: () => [],
    getElementById: () => null,
    addEventListener: () => {},
    removeEventListener: () => {},
    dispatchEvent: () => true,
    head: { appendChild: () => {}, removeChild: () => {} },
  };
  // In Node.js >= 21 `navigator` is a built-in read-only getter on globalThis,
  // so a plain assignment throws. Use Object.defineProperty to safely provide
  // a fallback only when the property does not already exist.
  if (typeof globalThis.navigator === "undefined") {
    Object.defineProperty(globalThis, "navigator", {
      value: { userAgent: "node" },
      writable: true,
      configurable: true,
    });
  }
  globalThis.CustomEvent = globalThis.CustomEvent || class CustomEvent extends Event {
    constructor(type, params = {}) { super(type); this.detail = params.detail || null; }
  };
  globalThis.matchMedia = globalThis.matchMedia || (() => ({
    matches: false,
    addEventListener: () => {},
    removeEventListener: () => {},
  }));
}

import { getRouteMatchInfo, render } from "./build-ssr/entry-server.js";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const clientBuildDir = path.join(__dirname, "build");
const indexHtmlPath = path.join(clientBuildDir, "index.html");
const indexHtml = await fs.readFile(indexHtmlPath, "utf-8");

function parseArgs(argv) {
  const args = argv.slice(2);
  let port = Number(process.env.PORT) || 4173;
  let strictPort = false;

  for (let i = 0; i < args.length; i++) {
    const arg = args[i];
    if (arg === "--port") {
      const portArg = args[i + 1];
      if (portArg !== undefined) {
        const parsedPort = Number(portArg);
        if (Number.isFinite(parsedPort) && Number.isInteger(parsedPort) && parsedPort >= 1 && parsedPort <= 65535) {
          port = parsedPort;
          i++;
        } else {
          console.error(`Invalid port value: ${portArg}. Using default port ${port}.`);
        }
      } else {
        console.error(`Missing port value after --port. Using default port ${port}.`);
      }
      continue;
    }
    if (arg === "--strictPort") {
      strictPort = true;
    }
  }

  return { port, strictPort };
}

function getContentType(filePath) {
  if (filePath.endsWith(".html")) return "text/html";
  if (filePath.endsWith(".js")) return "text/javascript";
  if (filePath.endsWith(".mjs")) return "text/javascript";
  if (filePath.endsWith(".css")) return "text/css";
  if (filePath.endsWith(".json")) return "application/json";
  if (filePath.endsWith(".svg")) return "image/svg+xml";
  if (filePath.endsWith(".png")) return "image/png";
  if (filePath.endsWith(".jpg") || filePath.endsWith(".jpeg")) return "image/jpeg";
  if (filePath.endsWith(".gif")) return "image/gif";
  if (filePath.endsWith(".webp")) return "image/webp";
  if (filePath.endsWith(".ico")) return "image/x-icon";
  if (filePath.endsWith(".map")) return "application/json";
  if (filePath.endsWith(".woff")) return "font/woff";
  if (filePath.endsWith(".woff2")) return "font/woff2";
  if (filePath.endsWith(".ttf")) return "font/ttf";
  return "application/octet-stream";
}

async function tryServeStatic(pathname, res) {
  if (pathname === "/" || pathname === "") {
    return false;
  }

  const relativePath = pathname.startsWith("/")
    ? pathname.slice(1)
    : pathname;

  const resolvedPath = path.join(clientBuildDir, relativePath);
  // Use path.relative to safely check if resolvedPath is inside clientBuildDir
  const relativeFromBuild = path.relative(clientBuildDir, resolvedPath);
  if (relativeFromBuild.startsWith("..") || path.isAbsolute(relativeFromBuild)) {
    return false;
  }

  try {
    const stat = await fs.stat(resolvedPath);
    if (!stat.isFile()) {
      return false;
    }
  } catch {
    return false;
  }

  res.statusCode = 200;
  res.setHeader("Content-Type", getContentType(resolvedPath));
  const stream = createReadStream(resolvedPath);
  stream.on("error", (err) => {
    console.error(`Error reading file ${resolvedPath}:`, err);
    if (!res.headersSent) {
      res.statusCode = 500;
      res.setHeader("Content-Type", "text/plain");
      res.end("Internal Server Error");
    } else {
      res.end();
    }
  });
  stream.pipe(res);
  return true;
}

async function handleRequest(req, res) {
  // Use a fixed base URL since we only need pathname and search params.
  // Avoids potential Host header injection if the URL were used elsewhere.
  const url = new URL(req.url ?? "/", "http://localhost");

  if (await tryServeStatic(url.pathname, res)) {
    return;
  }

  const routeInfo = getRouteMatchInfo(url.pathname);
  if (routeInfo.outsideBase) {
    res.statusCode = 200;
    res.setHeader("Content-Type", "text/html");
    res.end(indexHtml);
    return;
  }

  if (!routeInfo.matched) {
    // Inject a basic 404 message for users with JS disabled
    const notFoundHtml = indexHtml
      .replace("<!--ssr-outlet-->", '<div style="text-align:center;padding:2rem;"><h1>404</h1><p>Page not found</p></div>');
    res.statusCode = 404;
    res.setHeader("Content-Type", "text/html");
    res.end(notFoundHtml);
    return;
  }

  if (routeInfo.ssr) {
    const { appHtml, headHtml, hasPageTitle } = await render(url.pathname + url.search);
    let html = indexHtml;

    // If page has its own title, remove the global <title> to avoid duplicates
    if (hasPageTitle) {
      html = html.replace(/<title>[^<]*<\/title>/, "");
    }

    html = html
      .replace("<!--ssr-head-->", headHtml)
      .replace('<div id="root">', '<div id="root" data-wasp-ssr="1">')
      .replace("<!--ssr-outlet-->", appHtml);

    // Return 404 for catch-all routes (e.g., path: "*")
    res.statusCode = routeInfo.isCatchAll ? 404 : 200;
    res.setHeader("Content-Type", "text/html");
    res.end(html);
    return;
  }

  // Return 404 for catch-all routes even without SSR
  res.statusCode = routeInfo.isCatchAll ? 404 : 200;
  res.setHeader("Content-Type", "text/html");
  res.end(indexHtml);
}

const { port, strictPort } = parseArgs(process.argv);
const server = http.createServer((req, res) => {
  handleRequest(req, res).catch((error) => {
    console.error(error);
    res.statusCode = 500;
    res.setHeader("Content-Type", "text/plain");
    res.end("Internal Server Error");
  });
});

server.listen(port);

server.on("error", (error) => {
  if (error.code === "EADDRINUSE" && strictPort) {
    console.error(`Port ${port} is already in use.`);
    process.exit(1);
  }
  console.error(error);
  process.exit(1);
});

server.on("listening", () => {
  console.log(`SSR server listening on port ${port}`);
});
