/** This module defines the user-facing API for defining a Wasp app.
 */
import type * as AppSpec from "../appSpec.js";
import { getModuleSpec, GET_TS_APP_SPEC } from "../_private.js";
import { AppDeclBuilder, Module } from "./Module.js";
import * as TsAppSpec from "./tsAppSpec.js";

export class App {
  static projectPackageName: string | undefined;

  #module: AppDeclBuilder;
  #app: { name: string; config: TsAppSpec.AppConfig };
  #auth?: TsAppSpec.AuthConfig;
  #client?: TsAppSpec.ClientConfig;
  #db?: TsAppSpec.DbConfig;
  #emailSender?: TsAppSpec.EmailSenderConfig;
  #server?: TsAppSpec.ServerConfig;
  #websocket?: TsAppSpec.WebsocketConfig;
  #moduleServerSetupFns: TsAppSpec.ExtImport[] = [];
  #moduleClientSetupFns: TsAppSpec.ExtImport[] = [];
  #moduleProvides: TsAppSpec.ModuleProvideEntry[] = [];
  #moduleEntityMaps: TsAppSpec.ModuleEntityMapEntry[] = [];
  #declarationSources = new Map<string, string>();

  // NOTE: Using a non-public symbol gives us a package-private property.
  // It's not that important to hide it from the users, but we still don't want
  // user's IDE to suggest it during autocompletion.
  [GET_TS_APP_SPEC](): TsAppSpec.TsAppSpec {
    const moduleSpec = getModuleSpec(this.#module);
    return {
      app: this.#app,
      actions: moduleSpec.actions,
      apiNamespaces: moduleSpec.apiNamespaces,
      apis: moduleSpec.apis,
      auth: this.#auth,
      client: this.#client,
      cruds: moduleSpec.cruds,
      db: this.#db,
      emailSender: this.#emailSender,
      jobs: moduleSpec.jobs,
      pages: moduleSpec.pages,
      queries: moduleSpec.queries,
      routes: moduleSpec.routes,
      provides: moduleSpec.provides,
      entityAliases: moduleSpec.entityAliases,
      entityDeclarations: moduleSpec.entityDeclarations,
      server: this.#server,
      websocket: this.#websocket,
      moduleServerSetupFns: this.#moduleServerSetupFns,
      moduleClientSetupFns: this.#moduleClientSetupFns,
      moduleProvides: this.#moduleProvides,
      moduleEntityMaps: this.#moduleEntityMaps,
    };
  }

  constructor(name: string, config: TsAppSpec.AppConfig) {
    this.#module = new AppDeclBuilder();
    this.#app = { name, config };
  }

  use(
    this: App,
    module: Module,
    config?: { entityMap?: Record<string, string> },
  ): void {
    const original = getModuleSpec(module);
    const current = getModuleSpec(this.#module);

    const packageName = original.packageName;
    const entityMap = config?.entityMap;

    // Validate entity map against module's declared entity aliases
    if (entityMap && original.entityAliases.size > 0) {
      const incomingSource = packageName ?? "anonymous module";
      for (const alias of original.entityAliases) {
        if (!entityMap[alias]) {
          throw new Error(
            `Module '${incomingSource}' declares entity '${alias}' but no mapping ` +
            `was provided. Add '${alias}' to the entityMap in app.use().`,
          );
        }
      }
      for (const alias of Object.keys(entityMap)) {
        if (!original.entityAliases.has(alias)) {
          throw new Error(
            `entityMap contains '${alias}' but module '${incomingSource}' ` +
            `does not declare an entity with that alias.`,
          );
        }
      }
    }

    // Map-typed declaration keys that get merged by name.
    // `provides`, `serverSetupFn`, and `clientSetupFn` are handled separately below.
    const mapKeys: (keyof TsAppSpec.TsModuleSpec)[] = [
      "actions",
      "apiNamespaces",
      "apis",
      "cruds",
      "jobs",
      "pages",
      "queries",
      "routes",
    ];

    // Clone maps so we don't mutate the original module spec.
    const incoming: TsAppSpec.TsModuleSpec = {
      ...original,
      provides: new Map(original.provides),
      serverSetupFn: original.serverSetupFn,
      clientSetupFn: original.clientSetupFn,
      entityAliases: new Set(original.entityAliases),
      entityDeclarations: new Map(original.entityDeclarations),
    };
    for (const key of mapKeys) {
      (incoming as any)[key] = new Map(original[key] as Map<string, unknown>);
    }

    // Rewrite entity aliases to real entity names in operations
    if (entityMap) {
      App.#rewriteEntityAliases(incoming, entityMap);
    }

    if (packageName && packageName !== App.projectPackageName) {
      App.#rewriteModuleImports(incoming, packageName);
    }

    const incomingSource = packageName ?? "anonymous module";

    for (const key of mapKeys) {
      const currentMap = current[key] as Map<string, unknown>;
      const incomingMap = incoming[key] as Map<string, unknown>;
      for (const [name, value] of incomingMap) {
        const sourceKey = `${key}:${name}`;
        if (currentMap.has(name)) {
          const existingSource = this.#declarationSources.get(sourceKey) ?? "app";
          throw new Error(
            `Duplicate ${key} declaration: '${name}' from '${incomingSource}' ` +
            `conflicts with '${name}' from '${existingSource}'.`,
          );
        }
        currentMap.set(name, value);
        this.#declarationSources.set(sourceKey, incomingSource);
      }
    }

    if (incoming.serverSetupFn) {
      this.#moduleServerSetupFns.push(incoming.serverSetupFn);
    }
    if (incoming.clientSetupFn) {
      this.#moduleClientSetupFns.push(incoming.clientSetupFn);
    }
    if (packageName) {
      this.#moduleProvides.push({
        packageName,
        values: Object.fromEntries(incoming.provides),
      });
    } else if (incoming.provides.size > 0) {
      throw new Error(
        `Cannot use provide() without a package name. ` +
          `Pass the package name to the Module constructor: new Module("@scope/my-module")`,
      );
    }

    // Store entity map for code generation
    if (packageName && entityMap && Object.keys(entityMap).length > 0) {
      this.#moduleEntityMaps.push({ packageName, entityMap });
    }
  }

  static #rewritePath(
    path: AppSpec.ExtImport["path"],
    packageName: string,
  ): AppSpec.ExtImport["path"] {
    if (path.startsWith("@src/")) {
      const suffix = path.slice("@src/".length);
      return `@pkg/${packageName}/${suffix}` as `@pkg/${string}`;
    }
    return path;
  }

  static #rewriteExtImport(
    extImport: TsAppSpec.ExtImport,
    packageName: string,
  ): TsAppSpec.ExtImport {
    return { ...extImport, from: App.#rewritePath(extImport.from, packageName) };
  }

  static #rewriteOptionalExtImport(
    extImport: TsAppSpec.ExtImport | undefined,
    packageName: string,
  ): TsAppSpec.ExtImport | undefined {
    return extImport ? App.#rewriteExtImport(extImport, packageName) : undefined;
  }

  static #rewriteModuleImports(
    spec: TsAppSpec.TsModuleSpec,
    packageName: string,
  ): void {
    for (const [name, config] of spec.actions) {
      spec.actions.set(name, {
        ...config,
        fn: App.#rewriteExtImport(config.fn, packageName),
      });
    }

    for (const [name, config] of spec.apiNamespaces) {
      spec.apiNamespaces.set(name, {
        ...config,
        middlewareConfigFn: App.#rewriteExtImport(
          config.middlewareConfigFn,
          packageName,
        ),
      });
    }

    for (const [name, config] of spec.apis) {
      spec.apis.set(name, {
        ...config,
        fn: App.#rewriteExtImport(config.fn, packageName),
        middlewareConfigFn: App.#rewriteOptionalExtImport(
          config.middlewareConfigFn,
          packageName,
        ),
      });
    }

    for (const [name, config] of spec.cruds) {
      const rewriteOp = (
        op: TsAppSpec.CrudOperationOptions | undefined,
      ): TsAppSpec.CrudOperationOptions | undefined => {
        if (!op) return undefined;
        return {
          ...op,
          overrideFn: App.#rewriteOptionalExtImport(
            op.overrideFn,
            packageName,
          ),
        };
      };
      spec.cruds.set(name, {
        ...config,
        operations: {
          get: rewriteOp(config.operations.get),
          getAll: rewriteOp(config.operations.getAll),
          create: rewriteOp(config.operations.create),
          update: rewriteOp(config.operations.update),
          delete: rewriteOp(config.operations.delete),
        },
      });
    }

    for (const [name, config] of spec.jobs) {
      spec.jobs.set(name, {
        ...config,
        perform: {
          ...config.perform,
          fn: App.#rewriteExtImport(config.perform.fn, packageName),
        },
      });
    }

    for (const [name, config] of spec.pages) {
      spec.pages.set(name, {
        ...config,
        component: App.#rewriteExtImport(config.component, packageName),
      });
    }

    for (const [name, config] of spec.queries) {
      spec.queries.set(name, {
        ...config,
        fn: App.#rewriteExtImport(config.fn, packageName),
      });
    }

    if (spec.serverSetupFn) {
      spec.serverSetupFn = App.#rewriteExtImport(
        spec.serverSetupFn,
        packageName,
      );
    }
    if (spec.clientSetupFn) {
      spec.clientSetupFn = App.#rewriteExtImport(
        spec.clientSetupFn,
        packageName,
      );
    }
  }

  static #rewriteEntityAliases(
    spec: TsAppSpec.TsModuleSpec,
    entityMap: Record<string, string>,
  ): void {
    const resolveEntities = (entities?: string[]): string[] | undefined => {
      if (!entities) return undefined;
      return entities.map((name) => entityMap[name] ?? name);
    };

    for (const [name, config] of spec.actions) {
      spec.actions.set(name, { ...config, entities: resolveEntities(config.entities) });
    }
    for (const [name, config] of spec.queries) {
      spec.queries.set(name, { ...config, entities: resolveEntities(config.entities) });
    }
    for (const [name, config] of spec.apis) {
      spec.apis.set(name, { ...config, entities: resolveEntities(config.entities) });
    }
    for (const [name, config] of spec.jobs) {
      spec.jobs.set(name, { ...config, entities: resolveEntities(config.entities) });
    }
    for (const [name, config] of spec.cruds) {
      spec.cruds.set(name, { ...config, entity: entityMap[config.entity] ?? config.entity });
    }
  }

  #trackSource(declType: string, name: string): void {
    this.#declarationSources.set(`${declType}:${name}`, "app");
  }

  // TODO: Enforce that all methods are covered in compile time
  action(this: App, name: string, config: TsAppSpec.ActionConfig): void {
    this.#trackSource("actions", name);
    this.#module.action(name, config);
  }

  apiNamespace(
    this: App,
    name: string,
    config: TsAppSpec.ApiNamespaceConfig,
  ): void {
    this.#trackSource("apiNamespaces", name);
    this.#module.apiNamespace(name, config);
  }

  api(this: App, name: string, config: TsAppSpec.ApiConfig): void {
    this.#trackSource("apis", name);
    this.#module.api(name, config);
  }

  auth(this: App, config: TsAppSpec.AuthConfig): void {
    this.#auth = config;
  }

  client(this: App, config: TsAppSpec.ClientConfig): void {
    this.#client = config;
  }

  crud(this: App, name: string, config: TsAppSpec.CrudConfig): void {
    this.#trackSource("cruds", name);
    this.#module.crud(name, config);
  }

  db(this: App, config: TsAppSpec.DbConfig): void {
    this.#db = config;
  }

  emailSender(this: App, config: TsAppSpec.EmailSenderConfig): void {
    this.#emailSender = config;
  }

  job(this: App, name: string, config: TsAppSpec.JobConfig): void {
    this.#trackSource("jobs", name);
    this.#module.job(name, config);
  }

  page(
    this: App,
    name: string,
    config: TsAppSpec.PageConfig,
  ): TsAppSpec.PageName {
    this.#trackSource("pages", name);
    return this.#module.page(name, config);
  }

  query(this: App, name: string, config: TsAppSpec.QueryConfig): void {
    this.#trackSource("queries", name);
    this.#module.query(name, config);
  }

  route(this: App, name: string, config: TsAppSpec.RouteConfig): void {
    this.#trackSource("routes", name);
    this.#module.route(name, config);
  }

  server(this: App, config: TsAppSpec.ServerConfig): void {
    this.#server = config;
  }

  webSocket(this: App, config: TsAppSpec.WebsocketConfig) {
    this.#websocket = config;
  }
}
