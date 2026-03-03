import { name } from "../package.json" with { type: "json" };

export const PLUGIN_NAME = name as `$PLUGIN_NAME`;

export interface Options {
  clientEntrySrc: string;
  ssrEntrySrc: string;
  ssrPaths: readonly string[];
  ssrFallbackPath: string;
}
