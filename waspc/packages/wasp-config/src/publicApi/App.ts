/** This module defines the user-facing API for defining a Wasp app.
 */
import { GET_TS_APP_SPEC } from "../_private.js";
import {
  ActionConfig,
  ApiConfig,
  ApiNamespaceConfig,
  AppConfig,
  AuthConfig,
  ClientConfig,
  CrudConfig,
  DbConfig,
  EmailSenderConfig,
  JobConfig,
  PageConfig,
  PageName,
  QueryConfig,
  RouteConfig,
  ServerConfig,
  TsAppSpec,
  WebsocketConfig,
} from "./tsAppSpec.js";

export class App {
  #tsAppSpec: TsAppSpec;

  // NOTE: Using a non-public symbol gives us a package-private property.
  // It's not that important to hide it from the users, but we still don't want
  // user's IDE to suggest it during autocompletion.
  [GET_TS_APP_SPEC](): TsAppSpec {
    return this.#tsAppSpec;
  }

  constructor(name: string, config: AppConfig) {
    this.#tsAppSpec = {
      app: { name, config },
      actions: new Map<string, ActionConfig>(),
      apiNamespaces: new Map<string, ApiNamespaceConfig>(),
      apis: new Map<string, ApiConfig>(),
      auth: undefined,
      client: undefined,
      cruds: new Map<string, CrudConfig>(),
      db: undefined,
      emailSender: undefined,
      jobs: new Map<string, JobConfig>(),
      pages: new Map<string, PageConfig>(),
      queries: new Map<string, QueryConfig>(),
      routes: new Map<string, RouteConfig>(),
      server: undefined,
      websocket: undefined,
    };
  }

  // TODO: Enforce that all methods are covered in compile time
  action(this: App, name: string, config: ActionConfig): void {
    this.#tsAppSpec.actions.set(name, config);
  }

  apiNamespace(this: App, name: string, config: ApiNamespaceConfig): void {
    this.#tsAppSpec.apiNamespaces.set(name, config);
  }

  api(this: App, name: string, config: ApiConfig): void {
    this.#tsAppSpec.apis.set(name, config);
  }

  auth(this: App, config: AuthConfig): void {
    this.#tsAppSpec.auth = config;
  }

  client(this: App, config: ClientConfig): void {
    this.#tsAppSpec.client = config;
  }

  crud(this: App, name: string, config: CrudConfig): void {
    this.#tsAppSpec.cruds.set(name, config);
  }

  db(this: App, config: DbConfig): void {
    this.#tsAppSpec.db = config;
  }

  emailSender(this: App, config: EmailSenderConfig): void {
    this.#tsAppSpec.emailSender = config;
  }

  job(this: App, name: string, config: JobConfig): void {
    this.#tsAppSpec.jobs.set(name, config);
  }

  page(this: App, name: string, config: PageConfig): PageName {
    this.#tsAppSpec.pages.set(name, config);
    return name as PageName;
  }

  query(this: App, name: string, config: QueryConfig): void {
    this.#tsAppSpec.queries.set(name, config);
  }

  route(this: App, name: string, config: RouteConfig): void {
    this.#tsAppSpec.routes.set(name, config);
  }

  server(this: App, config: ServerConfig): void {
    this.#tsAppSpec.server = config;
  }

  webSocket(this: App, config: WebsocketConfig) {
    this.#tsAppSpec.websocket = config;
  }
}
