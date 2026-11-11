import type { IncomingMessage } from "node:http";
import type { Connect, NormalizedHotChannel, Plugin } from "vite";
import {
  SERVER_ERROR_CONSOLE_PREFIX,
  SERVER_ERROR_EVENT,
  SERVER_ERROR_PATH,
  type ServerErrorPayload,
} from "../../../devServerErrors/shared.js";

const MAX_BODY_BYTES = 1024 * 1024;
const LOOPBACK_ADDRESSES = new Set(["127.0.0.1", "::1", "::ffff:127.0.0.1"]);

/**
 * Receives the errors the Wasp server reports in development and pushes them
 * to the browser over the HMR channel (see `logServerErrorsInConsole`).
 *
 * It must run before Vite's `vite:forward-console` plugin (so, with
 * `enforce: "pre"`), to stop Vite from forwarding these errors back to the
 * terminal once the browser logs them.
 */
export function serverErrors(): Plugin {
  return {
    name: "wasp:server-errors",
    apply: "serve",
    enforce: "pre",
    configureServer(server) {
      const clientHot = server.environments.client.hot;
      ignoreForwardedServerErrors(clientHot);

      const endpointPath = server.config.base + SERVER_ERROR_PATH;
      server.middlewares.use(async (req, res, next) => {
        if (req.url !== endpointPath) {
          return next();
        }
        // Only the Wasp server, running on this machine, may send errors. The
        // JSON content type also keeps other websites out: browsers won't
        // send it cross-origin without a CORS preflight, which we don't allow.
        if (
          req.method !== "POST" ||
          !isLoopback(req) ||
          req.headers["content-type"] !== "application/json"
        ) {
          res.statusCode = 403;
          return res.end();
        }

        const payload = await readPayload(req);
        if (!payload) {
          res.statusCode = 400;
          return res.end();
        }

        clientHot.send(SERVER_ERROR_EVENT, payload);
        res.statusCode = 204;
        res.end();
      });
    },
  };
}

/**
 * Vite's `forwardConsole` option sends the browser's `console.error` calls to
 * the terminal. The server errors we log in the browser are already in the
 * terminal, so we filter them out before Vite's listener sees them.
 */
function ignoreForwardedServerErrors(hot: NormalizedHotChannel): void {
  const on = hot.on.bind(hot) as (
    event: string,
    listener: (...args: unknown[]) => void,
  ) => void;

  hot.on = ((event: string, listener: (...args: unknown[]) => void) => {
    if (event !== "vite:forward-console") {
      return on(event, listener);
    }
    on(event, (payload, ...rest) => {
      if (!isServerErrorConsoleLog(payload)) {
        listener(payload, ...rest);
      }
    });
  }) as NormalizedHotChannel["on"];
}

function isServerErrorConsoleLog(payload: unknown): boolean {
  const { type, data } = payload as {
    type?: unknown;
    data?: { message?: unknown };
  };
  return (
    type === "log" &&
    typeof data?.message === "string" &&
    data.message.startsWith(SERVER_ERROR_CONSOLE_PREFIX)
  );
}

function isLoopback(req: Connect.IncomingMessage): boolean {
  const address = req.socket.remoteAddress;
  return address !== undefined && LOOPBACK_ADDRESSES.has(address);
}

async function readPayload(
  req: IncomingMessage,
): Promise<ServerErrorPayload | null> {
  let body = "";
  for await (const chunk of req) {
    body += chunk;
    if (body.length > MAX_BODY_BYTES) {
      return null;
    }
  }

  try {
    const { message } = JSON.parse(body) as { message?: unknown };
    return typeof message === "string" ? { message } : null;
  } catch {
    return null;
  }
}
