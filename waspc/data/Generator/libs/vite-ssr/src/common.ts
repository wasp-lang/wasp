import { name as PLUGIN_NAME } from "../package.json" with { type: "json" };

export { PLUGIN_NAME };

export interface Options {
  clientEntrySrc: string;
  ssrEntrySrc: string;
  ssrPaths: readonly string[];
  ssrFallbackPath: string;
}
