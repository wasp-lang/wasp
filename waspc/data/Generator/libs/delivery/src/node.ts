import express, {
  type Application,
  type CookieOptions,
  type Request,
  type RequestHandler,
  type Response,
  type Router,
} from "express";
import { randomBytes } from "node:crypto";
import {
  resolveWaspApiPath,
  resolveWaspApiUrl,
  type AppDeliveryConfig,
} from "./index";

const sessionCookieName = "wasp_session";
const csrfCookieName = "wasp_csrf";
const csrfHeaderName = "X-Wasp-CSRF";

export type AppRoutes = {
  waspApi: Router;
  custom: Router;
  serverRoot: Router;
};

export type ClientAssets = {
  directory: string;
  fallbackFile: string;
};

export type SocketHandshake = {
  headers: { cookie?: string; origin?: string };
  auth: { sessionId?: unknown };
};

export type SocketServerOptions = {
  cors: {
    origin: string;
    credentials: true;
  };
  allowRequest: (
    request: { headers: { origin?: string } },
    callback: (error: null, allowed: boolean) => void,
  ) => void;
};

export type DevelopmentProxy = Record<
  string,
  {
    target: string;
    changeOrigin: true;
    ws: true;
  }
>;

export type AppDelivery = {
  waspApiPath(routePath: string): string;
  waspApiUrl(routePath: string): string;
  mount(options: {
    app: Application;
    routes: AppRoutes;
    clientAssets?: ClientAssets;
  }): void;
  respondWithSession(response: Response, sessionId: string): void;
  clearSessionCredential(response: Response): void;
  handleInvalidHttpSessionCredential(
    response: Response,
  ): "continueUnauthenticated" | "reject";
  readHttpSessionCredential(request: Request): string | null;
  readSocketSessionCredential(handshake: SocketHandshake): string | null;
  socketServerOptions(frontendOrigin: string): SocketServerOptions;
  developmentProxy(target: string, customApiPaths: string[]): DevelopmentProxy;
};

export function configureAppDelivery(config: AppDeliveryConfig): AppDelivery {
  return {
    waspApiPath: (routePath) => resolveWaspApiPath(config, routePath),
    waspApiUrl: (routePath) =>
      resolveWaspApiUrl(config, config.serverUrl, routePath),
    mount: ({ app, routes, clientAssets }) => {
      if (config.authEnabled && config.mode === "integrated") {
        app.use(createRequestProtection());
      }

      app.use(routes.custom);
      app.get("/health", (_request, response) => {
        response.status(200).json({ status: "ok" });
      });

      if (config.mode === "split") {
        app.use(routes.serverRoot);
      }
      app.use(config.waspApiMountPath, routes.waspApi);

      if (clientAssets && config.serveClientAssets) {
        installClientAssets(app, clientAssets, config.waspApiMountPath);
      }
    },
    respondWithSession: (response, sessionId) => {
      if (config.mode === "integrated") {
        response.cookie(sessionCookieName, sessionId, sessionCookieOptions());
      }
    },
    clearSessionCredential: (response) => {
      if (config.mode === "integrated") {
        response.clearCookie(sessionCookieName, sessionCookieOptions());
      }
    },
    handleInvalidHttpSessionCredential: (response) => {
      if (config.mode === "integrated") {
        response.clearCookie(sessionCookieName, sessionCookieOptions());
        return "continueUnauthenticated";
      }

      return "reject";
    },
    readHttpSessionCredential: (request) => {
      if (config.mode === "integrated") {
        return readCookie(request.headers.cookie, sessionCookieName);
      }

      return readBearerCredential(request.get("Authorization"));
    },
    readSocketSessionCredential: (handshake) => {
      if (config.mode === "integrated") {
        return readCookie(handshake.headers.cookie, sessionCookieName);
      }

      return typeof handshake.auth.sessionId === "string"
        ? handshake.auth.sessionId
        : null;
    },
    socketServerOptions: (frontendOrigin) => {
      const origin = new URL(frontendOrigin).origin;
      return {
        cors: { origin, credentials: true },
        allowRequest: (request, callback) => {
          callback(
            null,
            request.headers.origin === undefined ||
              request.headers.origin === origin,
          );
        },
      };
    },
    developmentProxy: (target, customApiPaths) => {
      if (config.mode === "split") {
        return {};
      }

      const proxy = { target, changeOrigin: true as const, ws: true as const };
      return Object.fromEntries(
        [config.waspApiMountPath, "/socket.io", ...customApiPaths].map(
          (pathName) => [pathName, proxy],
        ),
      );
    },
  };
}

function createRequestProtection(): RequestHandler {
  return (request, response, next) => {
    const sessionCookie = readCookie(request.headers.cookie, sessionCookieName);
    const csrfCookie = readCookie(request.headers.cookie, csrfCookieName);

    if (!csrfCookie) {
      response.append(
        "Set-Cookie",
        serializeCookie(csrfCookieName, randomToken()),
      );
    }

    if (sessionCookie && isUnsafeMethod(request.method)) {
      const csrfHeader = request.get(csrfHeaderName);
      if (!csrfHeader || csrfHeader !== csrfCookie) {
        response.status(403).json({ message: "Invalid CSRF token" });
        return;
      }
    }

    next();
  };
}

function installClientAssets(
  app: Application,
  assets: ClientAssets,
  waspApiMountPath: string,
): void {
  app.use(express.static(assets.directory, { index: false }));
  app.use((request, response, next) => {
    if (request.method !== "GET" && request.method !== "HEAD") {
      next();
      return;
    }

    if (!request.get("Accept")?.includes("text/html")) {
      next();
      return;
    }

    if (
      request.path === "/health" ||
      request.path === waspApiMountPath ||
      request.path.startsWith(`${waspApiMountPath}/`)
    ) {
      next();
      return;
    }

    response.sendFile(assets.fallbackFile, { root: assets.directory });
  });
}

function readBearerCredential(header: string | undefined): string | null {
  const prefix = "Bearer ";
  return header?.startsWith(prefix) ? header.slice(prefix.length) : null;
}

function readCookie(header: string | undefined, name: string): string | null {
  const prefix = `${name}=`;
  const value = header
    ?.split(";")
    .map((part) => part.trim())
    .find((part) => part.startsWith(prefix));
  if (!value) {
    return null;
  }

  const encodedValue = value.slice(prefix.length);
  try {
    return decodeURIComponent(encodedValue);
  } catch {
    return encodedValue;
  }
}

function isUnsafeMethod(method: string): boolean {
  return method !== "GET" && method !== "HEAD" && method !== "OPTIONS";
}

function randomToken(): string {
  return randomBytes(32).toString("hex");
}

function sessionCookieOptions(): CookieOptions {
  return {
    httpOnly: true,
    path: "/",
    sameSite: "lax",
    secure: process.env.NODE_ENV === "production",
  };
}

function serializeCookie(
  name: string,
  value: string,
  httpOnly = false,
): string {
  return `${name}=${encodeURIComponent(value)}; ${httpOnly ? "HttpOnly; " : ""}Path=/; SameSite=Lax${process.env.NODE_ENV === "production" ? "; Secure" : ""}`;
}
