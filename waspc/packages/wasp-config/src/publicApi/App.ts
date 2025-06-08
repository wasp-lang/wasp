/** This module defines the user-facing API for defining a Wasp app.
 */
import { GET_TS_APP_SPEC } from "../_private.js";
import * as TsAppSpec from "./tsAppSpec.js";

export class App {
  #tsAppSpec: AppData;

  // NOTE: Using a non-public symbol gives us a package-private property.
  // It's not that important to hide it from the users, but we still don't want
  // user's IDE to suggest it during autocompletion.
  [GET_TS_APP_SPEC](): AppData {
    return this.#tsAppSpec;
  }

  constructor(name: string, config: TsAppSpec.AppConfig) {
    this.#tsAppSpec = {
      app: { name, config },
      actions: new Map<string, TsAppSpec.ActionConfig>(),
      apiNamespaces: new Map<string, TsAppSpec.ApiNamespaceConfig>(),
      apis: new Map<string, TsAppSpec.ApiConfig>(),
      auth: undefined,
      client: undefined,
      cruds: new Map<string, TsAppSpec.CrudConfig>(),
      db: undefined,
      emailSender: undefined,
      jobs: new Map<string, TsAppSpec.JobConfig>(),
      pages: new Map<string, TsAppSpec.PageConfig>(),
      queries: new Map<string, TsAppSpec.QueryConfig>(),
      routes: new Map<string, TsAppSpec.RouteConfig>(),
      server: undefined,
      websocket: undefined,
    };
  }

  // TODO: Enforce that all methods are covered in compile time
  action(this: App, name: string, config: TsAppSpec.ActionConfig): void {
    this.#tsAppSpec.actions.set(name, config);
  }

  apiNamespace(
    this: App,
    name: string,
    config: TsAppSpec.ApiNamespaceConfig,
  ): void {
    this.#tsAppSpec.apiNamespaces.set(name, config);
  }

  api(this: App, name: string, config: TsAppSpec.ApiConfig): void {
    this.#tsAppSpec.apis.set(name, config);
  }

  auth(this: App, config: TsAppSpec.AuthConfig): void {
    this.#tsAppSpec.auth = config;
  }

  client(this: App, config: TsAppSpec.ClientConfig): void {
    this.#tsAppSpec.client = config;
  }

  crud(this: App, name: string, config: TsAppSpec.CrudConfig): void {
    this.#tsAppSpec.cruds.set(name, config);
  }

  db(this: App, config: TsAppSpec.DbConfig): void {
    this.#tsAppSpec.db = config;
  }

  emailSender(this: App, config: TsAppSpec.EmailSenderConfig): void {
    this.#tsAppSpec.emailSender = config;
  }

  job(this: App, name: string, config: TsAppSpec.JobConfig): void {
    this.#tsAppSpec.jobs.set(name, config);
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

  route(this: App, name: string, config: TsAppSpec.RouteConfig): void {
    this.#tsAppSpec.routes.set(name, config);
  }

  server(this: App, config: TsAppSpec.ServerConfig): void {
    this.#tsAppSpec.server = config;
  }

  webSocket(this: App, config: TsAppSpec.WebsocketConfig) {
    this.#tsAppSpec.websocket = config;
  }
}

// TODO: new name
export type AppData = {
  app: { name: string; config: TsAppSpec.AppConfig };
  actions: Map<string, TsAppSpec.ActionConfig>;
  apiNamespaces: Map<string, TsAppSpec.ApiNamespaceConfig>;
  apis: Map<string, TsAppSpec.ApiConfig>;
  auth?: TsAppSpec.AuthConfig;
  client?: TsAppSpec.ClientConfig;
  cruds: Map<string, TsAppSpec.CrudConfig>;
  db?: TsAppSpec.DbConfig;
  emailSender?: TsAppSpec.EmailSenderConfig;
  jobs: Map<string, TsAppSpec.JobConfig>;
  pages: Map<string, TsAppSpec.PageConfig>;
  queries: Map<string, TsAppSpec.QueryConfig>;
  routes: Map<string, TsAppSpec.RouteConfig>;
  server?: TsAppSpec.ServerConfig;
  websocket?: TsAppSpec.WebsocketConfig;
};
