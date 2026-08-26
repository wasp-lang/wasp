import { Application, Request, Response, Router } from "express";

//#region src/index.d.ts
type AppDeliveryMode = "integrated" | "split";
type AppDeliveryConfig = Readonly<{
  mode: AppDeliveryMode;
  serverUrl: string;
  waspApiMountPath: string;
  authEnabled: boolean;
  serveClientAssets: boolean;
}>;
//#endregion
//#region src/node.d.ts
type AppRoutes = {
  waspApi: Router;
  custom: Router;
  serverRoot: Router;
};
type ClientAssets = {
  directory: string;
  fallbackFile: string;
};
type SocketHandshake = {
  headers: {
    cookie?: string;
    origin?: string;
  };
  auth: {
    sessionId?: unknown;
  };
};
type SocketServerOptions = {
  cors: {
    origin: string;
    credentials: true;
  };
  allowRequest: (request: {
    headers: {
      origin?: string;
    };
  }, callback: (error: null, allowed: boolean) => void) => void;
};
type DevelopmentProxy = Record<string, {
  target: string;
  changeOrigin: true;
  ws: true;
}>;
type AppDelivery = {
  waspApiPath(routePath: string): string;
  waspApiUrl(routePath: string): string;
  mount(options: {
    app: Application;
    routes: AppRoutes;
    clientAssets?: ClientAssets;
  }): void;
  respondWithSession(response: Response, sessionId: string): void;
  clearSessionCredential(response: Response): void;
  handleInvalidHttpSessionCredential(response: Response): "continueUnauthenticated" | "reject";
  readHttpSessionCredential(request: Request): string | null;
  readSocketSessionCredential(handshake: SocketHandshake): string | null;
  socketServerOptions(frontendOrigin: string): SocketServerOptions;
  developmentProxy(target: string, customApiPaths: string[]): DevelopmentProxy;
};
declare function configureAppDelivery(config: AppDeliveryConfig): AppDelivery;
//#endregion
export { AppDelivery, AppRoutes, ClientAssets, DevelopmentProxy, SocketHandshake, SocketServerOptions, configureAppDelivery };
//# sourceMappingURL=node.d.ts.map