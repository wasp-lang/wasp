export type AppDeliveryMode = "integrated" | "split";

export type AppDeliveryConfig = Readonly<{
  mode: AppDeliveryMode;
  serverUrl: string;
  waspApiMountPath: string;
  authEnabled: boolean;
  serveClientAssets: boolean;
}>;

export function resolveWaspApiPath(
  config: Pick<AppDeliveryConfig, "waspApiMountPath">,
  routePath: string,
): string {
  const prefix = config.waspApiMountPath.replace(/\/$/, "");
  const path = routePath.startsWith("/") ? routePath : `/${routePath}`;
  return `${prefix}${path}`;
}

export function resolveWaspApiUrl(
  config: Pick<AppDeliveryConfig, "waspApiMountPath">,
  serverUrl: string,
  routePath: string,
): string {
  return `${serverUrl.replace(/\/$/, "")}${resolveWaspApiPath(config, routePath)}`;
}
