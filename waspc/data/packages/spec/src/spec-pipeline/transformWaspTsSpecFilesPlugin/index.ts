import type { Plugin } from "rolldown";
import { transformRefImportsPlugin } from "./imports/index.js";
import {
  type GetRefOrigin,
  transformRefHelperPlugin,
} from "./refHelper/index.js";

export function transformWaspTsSpecFilesPlugin({
  getRefOrigin,
}: {
  getRefOrigin: GetRefOrigin;
}): Plugin[] {
  return [
    transformRefImportsPlugin(), // Ref imports transforms to the ref helper, so it should come first.
    transformRefHelperPlugin({ getRefOrigin }),
  ];
}
