import fs from "node:fs/promises";
import http from "node:http";
import path from "node:path";
import { fileURLToPath } from "node:url";
import sirv from "sirv";

// Provide minimal browser API stubs so that client-side code which accesses
// localStorage, sessionStorage, window, or document at module-init time does
// not crash during SSR. These stubs return safe no-op / empty values.
//
// NOTE: This makes `typeof window !== 'undefined'` evaluate to true during
// SSR, which defeats isBrowser guards in SDK files (e.g., operations/internal,
// WebSocketProvider, api/index). Current practical impact is low — operations
// aren't called during renderToString and socket.io fails silently. If more
// SSR-sensitive logic is added to the SDK, consider using a custom flag like
// `globalThis.__WASP_SSR__ = true` for SSR detection instead.
if (typeof globalThis.window === "undefined") {
  // ---------------------------------------------------------------------------
  // Storage (localStorage, sessionStorage)
  // ---------------------------------------------------------------------------
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

  // ---------------------------------------------------------------------------
  // Event system — react-router and other libs call window.addEventListener
  // ---------------------------------------------------------------------------
  globalThis.addEventListener = globalThis.addEventListener || (() => {});
  globalThis.removeEventListener = globalThis.removeEventListener || (() => {});
  globalThis.dispatchEvent = globalThis.dispatchEvent || (() => true);

  // ---------------------------------------------------------------------------
  // Location — axios reads window.location.href at module-init time
  // ---------------------------------------------------------------------------
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
    assign: () => {},
    replace: () => {},
    reload: () => {},
    toString: () => "http://localhost",
  };

  // ---------------------------------------------------------------------------
  // History — react-router and navigation libs access window.history
  // ---------------------------------------------------------------------------
  globalThis.history = {
    length: 1,
    state: null,
    scrollRestoration: "auto",
    pushState: () => {},
    replaceState: () => {},
    go: () => {},
    back: () => {},
    forward: () => {},
  };

  // ---------------------------------------------------------------------------
  // window = globalThis (must come after location/history so they're visible)
  // ---------------------------------------------------------------------------
  globalThis.window = globalThis;

  // ---------------------------------------------------------------------------
  // Document — comprehensive stub covering common DOM operations
  // ---------------------------------------------------------------------------
  const noopClassList = { add: () => {}, remove: () => {}, contains: () => false, toggle: () => false, replace: () => false, item: () => null, get length() { return 0; }, forEach: () => {}, entries: () => [][Symbol.iterator](), keys: () => [][Symbol.iterator](), values: () => [][Symbol.iterator]() };
  const noopStyle = new Proxy({}, { get: () => "", set: () => true });
  const makeElement = (tag) => ({
    tagName: (tag || "DIV").toUpperCase(),
    style: { ...noopStyle },
    dataset: {},
    setAttribute: () => {},
    getAttribute: () => null,
    removeAttribute: () => {},
    hasAttribute: () => false,
    appendChild: (c) => c,
    removeChild: (c) => c,
    insertBefore: (n) => n,
    replaceChild: (n) => n,
    cloneNode: () => makeElement(tag),
    contains: () => false,
    classList: { ...noopClassList },
    addEventListener: () => {},
    removeEventListener: () => {},
    dispatchEvent: () => true,
    querySelector: () => null,
    querySelectorAll: () => [],
    getElementsByTagName: () => [],
    getElementsByClassName: () => [],
    getBoundingClientRect: () => ({ top: 0, right: 0, bottom: 0, left: 0, width: 0, height: 0, x: 0, y: 0, toJSON: () => {} }),
    getAnimations: () => [],
    animate: () => ({ finished: Promise.resolve(), cancel: () => {}, play: () => {}, pause: () => {} }),
    focus: () => {},
    blur: () => {},
    click: () => {},
    innerHTML: "",
    outerHTML: "",
    textContent: "",
    innerText: "",
    children: [],
    childNodes: [],
    firstChild: null,
    lastChild: null,
    nextSibling: null,
    previousSibling: null,
    parentNode: null,
    parentElement: null,
    offsetWidth: 0,
    offsetHeight: 0,
    offsetTop: 0,
    offsetLeft: 0,
    clientWidth: 0,
    clientHeight: 0,
    scrollWidth: 0,
    scrollHeight: 0,
    scrollTop: 0,
    scrollLeft: 0,
  });

  globalThis.document = {
    documentElement: { ...makeElement("html"), ...{ classList: { ...noopClassList } } },
    body: { ...makeElement("body"), ...{ classList: { ...noopClassList } } },
    head: { appendChild: (c) => c, removeChild: (c) => c, insertBefore: (n) => n, children: [], querySelectorAll: () => [] },
    createElement: (tag) => makeElement(tag),
    createElementNS: (_ns, tag) => makeElement(tag),
    createTextNode: (text) => ({ textContent: text, nodeType: 3 }),
    createComment: (text) => ({ textContent: text, nodeType: 8 }),
    createDocumentFragment: () => ({ appendChild: (c) => c, children: [], querySelectorAll: () => [], querySelector: () => null }),
    querySelector: () => null,
    querySelectorAll: () => [],
    getElementById: () => null,
    getElementsByTagName: () => [],
    getElementsByClassName: () => [],
    addEventListener: () => {},
    removeEventListener: () => {},
    dispatchEvent: () => true,
    createEvent: (type) => new Event(type),
    cookie: "",
    title: "",
    readyState: "complete",
    hidden: false,
    visibilityState: "visible",
    hasFocus: () => false,
    activeElement: null,
    implementation: { createHTMLDocument: () => globalThis.document },
  };

  // ---------------------------------------------------------------------------
  // Navigator — Node.js >= 21 has a read-only navigator getter, so use
  // Object.defineProperty for safe fallback.
  // ---------------------------------------------------------------------------
  if (typeof globalThis.navigator === "undefined") {
    Object.defineProperty(globalThis, "navigator", {
      value: {
        userAgent: "node",
        language: "en",
        languages: ["en"],
        onLine: true,
        cookieEnabled: false,
        hardwareConcurrency: 1,
        maxTouchPoints: 0,
        clipboard: { readText: () => Promise.resolve(""), writeText: () => Promise.resolve() },
        mediaDevices: { enumerateDevices: () => Promise.resolve([]) },
        permissions: { query: () => Promise.resolve({ state: "denied" }) },
        serviceWorker: { ready: new Promise(() => {}), register: () => Promise.resolve(), getRegistrations: () => Promise.resolve([]) },
        sendBeacon: () => true,
        vibrate: () => false,
      },
      writable: true,
      configurable: true,
    });
  }

  // ---------------------------------------------------------------------------
  // Screen — analytics and responsive libs read window.screen
  // ---------------------------------------------------------------------------
  globalThis.screen = {
    width: 1920,
    height: 1080,
    availWidth: 1920,
    availHeight: 1080,
    colorDepth: 24,
    pixelDepth: 24,
    orientation: { type: "landscape-primary", angle: 0, addEventListener: () => {}, removeEventListener: () => {} },
  };

  // ---------------------------------------------------------------------------
  // Animation frame — framer-motion, react-spring, GSAP reference at init
  // ---------------------------------------------------------------------------
  let rafId = 0;
  globalThis.requestAnimationFrame = globalThis.requestAnimationFrame || ((cb) => setTimeout(cb, 16, ++rafId));
  globalThis.cancelAnimationFrame = globalThis.cancelAnimationFrame || ((id) => clearTimeout(id));
  globalThis.requestIdleCallback = globalThis.requestIdleCallback || ((cb) => setTimeout(() => cb({ didTimeout: false, timeRemaining: () => 50 }), 1));
  globalThis.cancelIdleCallback = globalThis.cancelIdleCallback || ((id) => clearTimeout(id));

  // ---------------------------------------------------------------------------
  // Observers — IntersectionObserver, ResizeObserver, MutationObserver
  // Used by lazy-loading, auto-sizing, and DOM-watching libraries.
  // ---------------------------------------------------------------------------
  const NoopObserver = class {
    constructor() {}
    observe() {}
    unobserve() {}
    disconnect() {}
    takeRecords() { return []; }
  };
  globalThis.IntersectionObserver = globalThis.IntersectionObserver || NoopObserver;
  globalThis.ResizeObserver = globalThis.ResizeObserver || NoopObserver;
  globalThis.MutationObserver = globalThis.MutationObserver || NoopObserver;

  // ---------------------------------------------------------------------------
  // CSS / layout — CSS-in-JS libs (Chakra, styled-components, emotion)
  // ---------------------------------------------------------------------------
  globalThis.getComputedStyle = globalThis.getComputedStyle || (() => new Proxy({}, { get: (_, prop) => prop === "getPropertyValue" ? () => "" : "" }));
  globalThis.getSelection = globalThis.getSelection || (() => ({ rangeCount: 0, addRange: () => {}, removeAllRanges: () => {}, getRangeAt: () => null, toString: () => "" }));

  // ---------------------------------------------------------------------------
  // Scroll — scroll libraries and react-router scroll restoration
  // ---------------------------------------------------------------------------
  globalThis.scrollTo = globalThis.scrollTo || (() => {});
  globalThis.scrollBy = globalThis.scrollBy || (() => {});
  globalThis.scroll = globalThis.scroll || (() => {});

  // ---------------------------------------------------------------------------
  // Viewport dimensions — responsive hooks, media queries
  // ---------------------------------------------------------------------------
  globalThis.innerWidth = globalThis.innerWidth || 1920;
  globalThis.innerHeight = globalThis.innerHeight || 1080;
  globalThis.outerWidth = globalThis.outerWidth || 1920;
  globalThis.outerHeight = globalThis.outerHeight || 1080;
  globalThis.devicePixelRatio = globalThis.devicePixelRatio || 1;
  globalThis.visualViewport = globalThis.visualViewport || {
    width: 1920, height: 1080, offsetLeft: 0, offsetTop: 0, pageLeft: 0, pageTop: 0, scale: 1,
    addEventListener: () => {}, removeEventListener: () => {},
  };

  // ---------------------------------------------------------------------------
  // Events & custom events
  // ---------------------------------------------------------------------------
  globalThis.CustomEvent = globalThis.CustomEvent || class CustomEvent extends Event {
    constructor(type, params = {}) { super(type); this.detail = params.detail || null; }
  };

  // ---------------------------------------------------------------------------
  // matchMedia — responsive / dark-mode detection
  // ---------------------------------------------------------------------------
  globalThis.matchMedia = globalThis.matchMedia || ((query) => ({
    matches: false,
    media: query,
    onchange: null,
    addEventListener: () => {},
    removeEventListener: () => {},
    addListener: () => {},    // deprecated but still used by older libs
    removeListener: () => {}, // deprecated but still used by older libs
    dispatchEvent: () => true,
  }));

  // ---------------------------------------------------------------------------
  // XMLHttpRequest — axios falls back to XHR if it detects a browser env
  // ---------------------------------------------------------------------------
  globalThis.XMLHttpRequest = globalThis.XMLHttpRequest || class XMLHttpRequest {
    open() {}
    send() {}
    abort() {}
    setRequestHeader() {}
    getResponseHeader() { return null; }
    getAllResponseHeaders() { return ""; }
    addEventListener() {}
    removeEventListener() {}
    get readyState() { return 0; }
    get status() { return 0; }
    get statusText() { return ""; }
    get responseText() { return ""; }
    get response() { return null; }
  };

  // ---------------------------------------------------------------------------
  // Miscellaneous APIs accessed at init by various popular libraries
  // ---------------------------------------------------------------------------
  globalThis.DOMParser = globalThis.DOMParser || class DOMParser {
    parseFromString() { return globalThis.document; }
  };
  globalThis.HTMLElement = globalThis.HTMLElement || class HTMLElement {};
  globalThis.HTMLIFrameElement = globalThis.HTMLIFrameElement || class HTMLIFrameElement {};
  globalThis.HTMLImageElement = globalThis.HTMLImageElement || class HTMLImageElement {};
  globalThis.SVGElement = globalThis.SVGElement || class SVGElement {};
  globalThis.Image = globalThis.Image || class Image { constructor() { this.src = ""; this.onload = null; this.onerror = null; } };
  globalThis.self = globalThis.self || globalThis;
  globalThis.top = globalThis.top || globalThis;
  globalThis.parent = globalThis.parent || globalThis;
  globalThis.frames = globalThis.frames || globalThis;
  globalThis.frameElement = globalThis.frameElement || null;
  globalThis.open = globalThis.open || (() => null);
  globalThis.close = globalThis.close || (() => {});
  globalThis.focus = globalThis.focus || (() => {});
  globalThis.blur = globalThis.blur || (() => {});
  globalThis.postMessage = globalThis.postMessage || (() => {});
  globalThis.alert = globalThis.alert || (() => {});
  globalThis.confirm = globalThis.confirm || (() => false);
  globalThis.prompt = globalThis.prompt || (() => null);
  globalThis.print = globalThis.print || (() => {});
  globalThis.origin = globalThis.origin || "http://localhost";
  globalThis.isSecureContext = globalThis.isSecureContext ?? false;
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
      .replace("<!--ssr-outlet-->", () => '<div style="text-align:center;padding:2rem;"><h1>404</h1><p>Page not found</p></div>');
    res.statusCode = 404;
    res.setHeader("Content-Type", "text/html");
    res.end(notFoundHtml);
    return;
  }

  if (routeInfo.ssr) {
    return render(url.pathname + url.search).then(({ appHtml, headHtml, hasPageTitle }) => {
      let html = indexHtml;

      // If page has its own title, remove the global <title> to avoid duplicates
      if (hasPageTitle) {
        html = html.replace(/<title>[^<]*<\/title>/, "");
      }

      html = html
        .replace("<!--ssr-head-->", () => headHtml)
        .replace('<div id="root">', '<div id="root" data-wasp-ssr="1">')
        .replace("<!--ssr-outlet-->", () => appHtml);

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
  }

  // Return 404 for catch-all routes even without SSR
  res.statusCode = routeInfo.isCatchAll ? 404 : 200;
  res.setHeader("Content-Type", "text/html");
  res.end(indexHtml);
}

function handleRequest(req, res) {
  return new Promise((resolve, reject) => {
    try {
      // Use a fixed base URL since we only need pathname and search params.
      // Avoids potential Host header injection if the URL were used elsewhere.
      const url = new URL(req.url ?? "/", "http://localhost");

      // Let sirv try to serve the static asset first.
      // If sirv doesn't find a matching file, the callback runs and we
      // fall through to SSR / SPA rendering.
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
