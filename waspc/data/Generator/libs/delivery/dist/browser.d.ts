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
//#region src/browser.d.ts
type DeliveryStorage = {
  set: (key: string, value: string) => void;
  get: (key: string) => unknown;
  remove: (key: string) => void;
  clear: () => void;
};
type SocketConnectionOptions = {
  withCredentials: boolean;
  auth: {
    sessionId: string | null;
  };
};
type BrowserAppDelivery = {
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
declare function configureBrowserAppDelivery(options: {
  config: Pick<AppDeliveryConfig, "mode" | "serverUrl" | "waspApiMountPath">;
  storage: DeliveryStorage;
}): BrowserAppDelivery;
//#endregion
export { BrowserAppDelivery, DeliveryStorage, SocketConnectionOptions, configureBrowserAppDelivery };
//# sourceMappingURL=browser.d.ts.map