/** This module defines the user-facing API for defining a Wasp app.
 */
import { GET_TS_APP_SPEC } from "../_private.js";
import * as TsAppSpec from "./tsAppSpec.js";

export class App {
  #tsAppSpec: TsAppSpec.TsAppSpec;

  // NOTE: Using a non-public symbol gives us a package-private property.
  // It's not that important to hide it from the users, but we still don't want
  // user's IDE to suggest it during autocompletion.
  [GET_TS_APP_SPEC](): TsAppSpec.TsAppSpec {
    return this.#tsAppSpec;
  }

  constructor(name: string, config: TsAppSpec.AppConfig) {
    this.#tsAppSpec = {
      app: { name, config },
      pages: new Map<string, TsAppSpec.PageConfig>(),
      queries: new Map<string, TsAppSpec.QueryConfig>(),
    };
  }

  page(
    this: App,
    name: string,
    config: TsAppSpec.PageConfig,
  ): TsAppSpec.PageName {
    this.#tsAppSpec.pages.set(name, config);
    return name as TsAppSpec.PageName;
  }

  query(this: App, name: string, config: TsAppSpec.QueryConfig): void {
    this.#tsAppSpec.queries.set(name, config);
  }
}
