export { App } from "./spec/publicApi/App.js";
export type {
  AppConfig,
  ExtImport,
  PageConfig,
  PageName,
  QueryConfig,
  /**
   * We don't want to export this type, as it's only used internally by the `App`.
   * Users can't access this type through the public API.
   */
  // TsAppSpec,
} from "./spec/publicApi/tsAppSpec.js";
