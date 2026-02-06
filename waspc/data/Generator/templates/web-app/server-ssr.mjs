import fs from "node:fs/promises";
import http from "node:http";
import path from "node:path";
import { fileURLToPath } from "node:url";
import sirv from "sirv";

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

// Dynamic import so that the browser API polyfills above are in place
// before the SSR bundle (and its transitive dependencies) are evaluated.
// A static `import` would be hoisted above the polyfill block by the
// ES module system, causing crashes in libraries that access window/document
// at module-init time.
const { getRouteMatchInfo, render } = await import("./build-ssr/entry-server.js");

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

// Serve static assets from the client build directory using sirv.
// sirv caches file-system lookups upfront for fast responses and provides
// ETag / 304 Not Modified support out of the box.
// We disable extension fallbacks (e.g. .html) since we handle routing ourselves.
const serveStatic = sirv(clientBuildDir, {
  etag: true,
  gzip: true,
  brotli: true,
  extensions: [],
  setHeaders: (res, pathname) => {
    // Vite-built assets have content hashes in filenames (e.g. assets/index-a1b2c3.js),
    // so they are safe to cache indefinitely. Other static files get a shorter cache.
    if (pathname.startsWith("/assets/")) {
      res.setHeader("Cache-Control", "public, max-age=31536000, immutable");
    } else {
      res.setHeader("Cache-Control", "public, max-age=3600");
    }
  },
});

function handleSsrRequest(url, res) {
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
    render(url.pathname + url.search).then(({ appHtml, headHtml, hasPageTitle }) => {
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
    }).catch((error) => {
      console.error("SSR render error:", error);
      if (!res.headersSent) {
        res.statusCode = 500;
        res.setHeader("Content-Type", "text/plain");
        res.end("Internal Server Error");
      }
    });
    return;
  }

  // Return 404 for catch-all routes even without SSR
  res.statusCode = routeInfo.isCatchAll ? 404 : 200;
  res.setHeader("Content-Type", "text/html");
  res.end(indexHtml);
}

function handleRequest(req, res) {
  // Use a fixed base URL since we only need pathname and search params.
  // Avoids potential Host header injection if the URL were used elsewhere.
  const url = new URL(req.url ?? "/", "http://localhost");

  // Let sirv try to serve the static asset first.
  // If sirv doesn't find a matching file, the callback runs and we
  // fall through to SSR / SPA rendering.
  serveStatic(req, res, () => {
    handleSsrRequest(url, res);
  });
}

const { port, strictPort } = parseArgs(process.argv);
const server = http.createServer((req, res) => {
  try {
    handleRequest(req, res);
  } catch (error) {
    console.error(error);
    res.statusCode = 500;
    res.setHeader("Content-Type", "text/plain");
    res.end("Internal Server Error");
  }
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
