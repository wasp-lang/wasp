import { readFile } from "node:fs/promises";

import type { IncomingMessage, ServerResponse } from "node:http";

import type { Plugin } from "vite";

const PLAYGROUND_ID = "wasp-api-playground";
const PLAYGROUND_BASE = "/__wasp-devtools/playground/";
// Relative to `PLAYGROUND_BASE`.
const PROXY_PATH = "/proxy/";

/**
 * Adds the API Playground panel to Vite DevTools: a Postman-like view for
 * calling the app's queries, actions, APIs, and jobs, and for watching the
 * requests the server handles. It talks to the dev-only `/__wasp/devtools`
 * routes of the Wasp server.
 */
export function apiPlayground(): Plugin {
  return {
    name: "wasp:devtools-api-playground",
    apply: "serve",
    devtools: {
      setup(ctx) {
        ctx.docks.register({
          id: PLAYGROUND_ID,
          title: "API Playground",
          icon: "ph:paper-plane-tilt-duotone",
          description: "Call your queries, actions, APIs, and jobs.",
          type: "iframe",
          url: PLAYGROUND_BASE,
          category: "framework",
          defaultOrder: -1,
        });
      },
    },
    configureServer(server) {
      const getServerUrl = () =>
        (
          server.config.env["REACT_APP_API_URL"] ??
          process.env["REACT_APP_API_URL"] ??
          "http://localhost:3001"
        ).replace(/\/$/, "");

      server.middlewares.use(PLAYGROUND_BASE, async (req, res, next) => {
        const url = new URL(req.url ?? "/", "http://x");
        const pathname = url.pathname;
        try {
          if (pathname === "/" || pathname === "/index.html") {
            res.setHeader("Content-Type", "text/html");
            res.setHeader("Cache-Control", "no-cache");
            res.end(renderPage({ serverUrl: getServerUrl() }));
            return;
          }
          if (pathname.startsWith(PROXY_PATH)) {
            // Forwards the playground's requests to the Wasp server, so they
            // work for APIs that don't allow the client's origin through CORS.
            await proxyToServer(
              req,
              res,
              getServerUrl() +
                pathname.slice(PROXY_PATH.length - 1) +
                url.search,
            );
            return;
          }
          if (pathname === "/playground.js") {
            const script = await readFile(
              new URL("./client/playground.js", import.meta.url),
            );
            res.setHeader("Content-Type", "text/javascript");
            res.setHeader("Cache-Control", "no-cache");
            res.end(script);
            return;
          }
        } catch {
          // Fall through to Vite.
        }
        next();
      });
    },
  };
}

function renderPage(config: { serverUrl: string }): string {
  return `<!doctype html>
<html lang="en">
  <head>
    <meta charset="utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <title>API Playground · Wasp DevTools</title>
    <script>window.__WASP_PLAYGROUND__ = ${JSON.stringify(config)};</script>
  </head>
  <body>
    <div id="app"></div>
    <script type="module" src="${PLAYGROUND_BASE}playground.js"></script>
  </body>
</html>`;
}

async function proxyToServer(
  req: IncomingMessage,
  res: ServerResponse,
  targetUrl: string,
): Promise<void> {
  const chunks: Buffer[] = [];
  for await (const chunk of req) chunks.push(chunk as Buffer);
  const headers = new Headers();
  for (const [key, value] of Object.entries(req.headers)) {
    if (value === undefined || HOP_BY_HOP_HEADERS.has(key)) continue;
    headers.set(key, Array.isArray(value) ? value.join(", ") : value);
  }
  try {
    const response = await fetch(targetUrl, {
      method: req.method ?? "GET",
      headers,
      body: chunks.length ? Buffer.concat(chunks) : undefined,
      redirect: "manual",
    });
    const body = Buffer.from(await response.arrayBuffer());
    res.statusCode = response.status;
    res.statusMessage = response.statusText;
    response.headers.forEach((value, key) => {
      // `fetch` already decoded the body.
      if (!HOP_BY_HOP_HEADERS.has(key) && key !== "content-encoding") {
        res.setHeader(key, value);
      }
    });
    res.setHeader("content-length", body.length);
    res.end(body);
  } catch (error) {
    res.statusCode = 502;
    res.setHeader("Content-Type", "application/json");
    res.end(
      JSON.stringify({
        message: `Couldn't reach the Wasp server at ${targetUrl}: ${error instanceof Error ? error.message : String(error)}`,
      }),
    );
  }
}

const HOP_BY_HOP_HEADERS = new Set([
  "host",
  "connection",
  "keep-alive",
  "transfer-encoding",
  "content-length",
  "upgrade",
  "origin",
  "referer",
]);
