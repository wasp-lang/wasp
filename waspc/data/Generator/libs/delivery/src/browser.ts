import {
  resolveWaspApiPath,
  resolveWaspApiUrl,
  type AppDeliveryConfig,
} from "./index";

export type DeliveryStorage = {
  set: (key: string, value: string) => void;
  get: (key: string) => unknown;
  remove: (key: string) => void;
  clear: () => void;
};

export type SocketConnectionOptions = {
  withCredentials: boolean;
  auth: { sessionId: string | null };
};

export type BrowserAppDelivery = {
  serverUrl: string;
  waspApiPath(routePath: string): string;
  waspApiUrl(routePath: string): string;
  prepareHttpRequest(request: Request): void;
  acceptSession(sessionId: string): void;
  clearSession(): void;
  clearLocalData(): void;
  currentSessionId(): string | null;
  sessionIdFromAuthorizationHeader(header: string | null): string | null;
  socketConnectionOptions(): SocketConnectionOptions;
};

const sessionStorageKey = "sessionId";

export function configureBrowserAppDelivery(options: {
  config: Pick<AppDeliveryConfig, "mode" | "serverUrl" | "waspApiMountPath">;
  storage: DeliveryStorage;
}): BrowserAppDelivery {
  const { config, storage } = options;
  const serverUrl =
    config.mode === "integrated" && typeof window !== "undefined"
      ? window.location.origin
      : config.serverUrl;
  const currentSessionId = () => {
    if (config.mode === "integrated") {
      return null;
    }

    const sessionId = storage.get(sessionStorageKey);
    return typeof sessionId === "string" ? sessionId : null;
  };

  return {
    serverUrl,
    waspApiPath: (routePath) => resolveWaspApiPath(config, routePath),
    waspApiUrl: (routePath) => resolveWaspApiUrl(config, serverUrl, routePath),
    prepareHttpRequest: (request) => {
      if (config.mode === "integrated") {
        const csrfToken = readBrowserCookie("wasp_csrf");
        if (csrfToken) {
          request.headers.set("X-Wasp-CSRF", csrfToken);
        }
        return;
      }

      const sessionId = currentSessionId();
      if (sessionId) {
        request.headers.set("Authorization", `Bearer ${sessionId}`);
      }
    },
    acceptSession: (sessionId) => {
      if (config.mode === "split") {
        storage.set(sessionStorageKey, sessionId);
      }
    },
    clearSession: () => {
      if (config.mode === "split") {
        storage.remove(sessionStorageKey);
      }
    },
    clearLocalData: () => {
      if (config.mode === "split") {
        storage.clear();
      }
    },
    currentSessionId,
    sessionIdFromAuthorizationHeader: (header) => {
      const prefix = "Bearer ";
      return header?.startsWith(prefix) ? header.slice(prefix.length) : null;
    },
    socketConnectionOptions: () => ({
      withCredentials: config.mode === "integrated",
      auth: { sessionId: currentSessionId() },
    }),
  };
}

function readBrowserCookie(name: string): string | null {
  if (typeof document === "undefined") {
    return null;
  }

  const prefix = `${name}=`;
  const value = document.cookie
    .split(";")
    .map((part) => part.trim())
    .find((part) => part.startsWith(prefix));
  return value ? decodeURIComponent(value.slice(prefix.length)) : null;
}
