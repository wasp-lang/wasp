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
  // Stub location so that libraries like axios which access
  // window.location.href at module-init time don't crash.
  globalThis.location = {
    href: "http://localhost",
    origin: "http://localhost",
    protocol: "http:",
    host: "localhost",
    hostname: "localhost",
    port: "",
    pathname: "/",
    search: "",
    hash: "",
  };
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
      addEventListener: () => {},
      removeEventListener: () => {},
    },
    createElement: () => ({
      style: {},
      setAttribute: () => {},
      getAttribute: () => null,
      appendChild: () => {},
      insertBefore: () => {},
      nextSibling: null,
      sheet: { cssRules: [], insertRule: () => 0, deleteRule: () => {} },
      classList: { add: () => {}, remove: () => {} },
    }),
    createTextNode: () => ({}),
    querySelector: () => null,
    querySelectorAll: () => [],
    getElementById: () => null,
    addEventListener: () => {},
    removeEventListener: () => {},
    dispatchEvent: () => true,
    head: { appendChild: () => {}, removeChild: () => {}, insertBefore: () => {}, firstChild: null, childNodes: [] },
    styleSheets: [],
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
  // Emotion's default cache context uses `typeof HTMLElement !== "undefined"`
  // to decide whether to create a default cache or set it to null. Without
  // this polyfill the cache is null, and MUI's styled() components crash with
  // "Cannot read properties of null (reading 'registered')".
  globalThis.HTMLElement = globalThis.HTMLElement || class HTMLElement {};
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
const serveStatic = sirv(clientBuildDir, {
  etag: true,
  gzip: true,
  brotli: true,
  extensions: [],
  setHeaders: (res, pathname) => {
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
  } else if (!routeInfo.matched) {
    const notFoundHtml = indexHtml
      .replace("<!--ssr-outlet-->", () => '<div style="text-align:center;padding:2rem;"><h1>404</h1><p>Page not found</p></div>');
    res.statusCode = 404;
    res.setHeader("Content-Type", "text/html");
    res.end(notFoundHtml);
  } else if (routeInfo.ssr) {
    return render(url.pathname + url.search).then(({ appHtml, headHtml, hasPageTitle }) => {
      let html = indexHtml;

      if (hasPageTitle) {
        html = html.replace(/<title>[^<]*<\/title>/, "");
      }

      html = html
        .replace("<!--ssr-head-->", () => headHtml)
        .replace('<div id="root">', '<div id="root" data-wasp-ssr="1">')
        .replace("<!--ssr-outlet-->", () => appHtml);

      res.statusCode = routeInfo.isCatchAll ? 404 : 200;
      res.setHeader("Content-Type", "text/html");
      res.end(html);
    }).catch((error) => {
      console.error("SSR render error:", error);
      // Fall back to the SPA shell so the client can render the page via
      // CSR instead of showing a blank "Internal Server Error" page.
      if (!res.headersSent) {
        res.statusCode = 200;
        res.setHeader("Content-Type", "text/html");
        res.end(indexHtml);
      }
    });
  } else {
    res.statusCode = routeInfo.isCatchAll ? 404 : 200;
    res.setHeader("Content-Type", "text/html");
    res.end(indexHtml);
  }
}

function handleRequest(req, res) {
  return new Promise((resolve, reject) => {
    try {
      const url = new URL(req.url ?? "/", "http://localhost");

      serveStatic(req, res, () => {
        try {
          Promise.resolve(handleSsrRequest(url, res)).then(resolve, reject);
        } catch (error) {
          reject(error);
        }
      });
    } catch (error) {
      reject(error);
    }
  });
}

const { port, strictPort } = parseArgs(process.argv);
const server = http.createServer((req, res) => {
  function send500(error) {
    console.error(error);
    if (!res.headersSent) {
      res.statusCode = 500;
      res.setHeader("Content-Type", "text/plain");
      res.end("Internal Server Error");
    }
  }

  try {
    Promise.resolve(handleRequest(req, res)).catch(send500);
  } catch (error) {
    send500(error);
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
