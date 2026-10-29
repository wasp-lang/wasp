import type { Plugin } from "rolldown";
import { transformRefImportsPlugin } from "./imports/index.js";
import { transformRefHelperPlugin } from "./refHelper/index.js";

export function transformWaspTsSpecFilesPlugin(): Plugin[] {
  return [
    transformRefImportsPlugin(), // Ref imports transforms to the ref helper, so it should come first.
    transformRefHelperPlugin(),
  ];
}
