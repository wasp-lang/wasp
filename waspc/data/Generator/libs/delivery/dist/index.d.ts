//#region src/index.d.ts
type AppDeliveryMode = "integrated" | "split";
type AppDeliveryConfig = Readonly<{
  mode: AppDeliveryMode;
  serverUrl: string;
  waspApiMountPath: string;
  authEnabled: boolean;
  serveClientAssets: boolean;
}>;
declare function resolveWaspApiPath(config: Pick<AppDeliveryConfig, "waspApiMountPath">, routePath: string): string;
declare function resolveWaspApiUrl(config: Pick<AppDeliveryConfig, "waspApiMountPath">, serverUrl: string, routePath: string): string;
//#endregion
export { AppDeliveryConfig, AppDeliveryMode, resolveWaspApiPath, resolveWaspApiUrl };
//# sourceMappingURL=index.d.ts.map