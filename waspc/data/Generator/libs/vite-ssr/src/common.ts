import { name as PLUGIN_NAME } from "../package.json" with { type: "json" };

export { PLUGIN_NAME };

export const CLIENT_APP_ENTRY_ID = `/${PLUGIN_NAME}/client-app-entry`;
export const CLIENT_HTML_ENTRY_ID = "index.html";

export interface Options {
  clientEntrySrc: string;
  ssrEntrySrc: string;
  ssrPaths: readonly string[];
  ssrFallbackPath: string;
}
