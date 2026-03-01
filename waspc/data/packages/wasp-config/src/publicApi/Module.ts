import { getModuleSpec, setModuleSpec } from "../_private.js";
import * as AppSpec from "../appSpec.js";
import * as TsAppSpec from "./tsAppSpec.js";

// --- Module-facing input types ---

export type ModuleExtImport =
  | { import: string; from: string }
  | { importDefault: string; from: string };

/**
 * Recursively replaces `ExtImport` fields with `ModuleExtImport` in a config type.
 * This allows module authors to use `@src/` paths in `from` fields.
 *
 * NOTE: Does not recurse into arrays (e.g. `ExtImport[]`). No module-level
 * config currently uses array-of-ExtImport fields, but if one is added this
 * type will need an array branch.
 */
export type ResolveExtImports<T> = {
  [K in keyof T]: T[K] extends TsAppSpec.ExtImport
    ? ModuleExtImport
    : T[K] extends TsAppSpec.ExtImport | undefined
      ? ModuleExtImport | undefined
      : NonNullable<T[K]> extends object
        ? ResolveExtImports<NonNullable<T[K]>> | Extract<T[K], undefined>
        : T[K];
};

// --- Module-level helpers (no private fields needed) ---

function resolveFrom(from: string): AppSpec.ExtImport["path"] {
  if (from.startsWith("@src/")) {
    return from as `@src/${string}`;
  }
  throw new Error(
    `Invalid import path "${from}". Must start with "@src/".`
  );
}

function resolveExtImport(extImport: ModuleExtImport): TsAppSpec.ExtImport {
  if ("import" in extImport) {
    return {
      import: extImport.import,
      from: resolveFrom(extImport.from),
    };
  }
  return {
    importDefault: extImport.importDefault,
    from: resolveFrom(extImport.from),
  };
}

function resolveOptionalExtImport(
  extImport: ModuleExtImport | undefined
): TsAppSpec.ExtImport | undefined {
  return extImport ? resolveExtImport(extImport) : undefined;
}

function resolveCrudOperations(
  ops: ResolveExtImports<TsAppSpec.CrudOperations>
): TsAppSpec.CrudOperations {
  const resolve = (
    op: ResolveExtImports<TsAppSpec.CrudOperationOptions> | undefined
  ): TsAppSpec.CrudOperationOptions | undefined => {
    if (!op) return undefined;
    return {
      ...op,
      overrideFn: resolveOptionalExtImport(op.overrideFn),
    };
  };
  return {
    get: resolve(ops.get),
    getAll: resolve(ops.getAll),
    create: resolve(ops.create),
    update: resolve(ops.update),
    delete: resolve(ops.delete),
  };
}

function spec(module: AppDeclBuilder): TsAppSpec.TsModuleSpec {
  return getModuleSpec(module);
}

// --- AppDeclBuilder (internal: packageName optional) ---

export class AppDeclBuilder {
  constructor(packageName?: string) {
    setModuleSpec(this, {
      actions: new Map(),
      apiNamespaces: new Map(),
      apis: new Map(),
      cruds: new Map(),
      jobs: new Map(),
      pages: new Map(),
      queries: new Map(),
      routes: new Map(),
      provides: new Map(),
      packageName,
      entityAliases: new Set(),
      entityDeclarations: new Map(),
    });
  }

  entity(alias: string, config?: { fields?: Record<string, string> }): void {
    const s = spec(this);
    if (s.entityAliases.has(alias)) {
      throw new Error(`Entity alias '${alias}' is already declared on this module.`);
    }
    s.entityAliases.add(alias);
    if (config?.fields) {
      s.entityDeclarations.set(alias, { fields: config.fields });
    }
  }

  requiresAuth(): void {
    spec(this).requiresAuth = true;
  }

  action(name: string, config: ResolveExtImports<TsAppSpec.ActionConfig>): void {
    spec(this).actions.set(name, {
      ...config,
      fn: resolveExtImport(config.fn),
    });
  }

  apiNamespace(
    name: string,
    config: ResolveExtImports<TsAppSpec.ApiNamespaceConfig>
  ): void {
    spec(this).apiNamespaces.set(name, {
      ...config,
      middlewareConfigFn: resolveExtImport(config.middlewareConfigFn),
    });
  }

  api(name: string, config: ResolveExtImports<TsAppSpec.ApiConfig>): void {
    spec(this).apis.set(name, {
      ...config,
      fn: resolveExtImport(config.fn),
      middlewareConfigFn: resolveOptionalExtImport(
        config.middlewareConfigFn
      ),
    });
  }

  crud(name: string, config: ResolveExtImports<TsAppSpec.CrudConfig>): void {
    spec(this).cruds.set(name, {
      ...config,
      operations: resolveCrudOperations(config.operations),
    });
  }

  job(name: string, config: ResolveExtImports<TsAppSpec.JobConfig>): void {
    spec(this).jobs.set(name, {
      ...config,
      perform: {
        ...config.perform,
        fn: resolveExtImport(config.perform.fn),
      },
    });
  }

  page(
    name: string,
    config: ResolveExtImports<TsAppSpec.PageConfig>
  ): TsAppSpec.PageName {
    spec(this).pages.set(name, {
      ...config,
      component: resolveExtImport(config.component),
    });
    return name as TsAppSpec.PageName;
  }

  query(name: string, config: ResolveExtImports<TsAppSpec.QueryConfig>): void {
    spec(this).queries.set(name, {
      ...config,
      fn: resolveExtImport(config.fn),
    });
  }

  route(name: string, config: TsAppSpec.RouteConfig): void {
    spec(this).routes.set(name, config);
  }

  serverSetup(fn: ModuleExtImport): void {
    if (spec(this).serverSetupFn) {
      throw new Error("serverSetup() has already been called on this module.");
    }
    spec(this).serverSetupFn = resolveExtImport(fn);
  }

  clientSetup(fn: ModuleExtImport): void {
    if (spec(this).clientSetupFn) {
      throw new Error("clientSetup() has already been called on this module.");
    }
    spec(this).clientSetupFn = resolveExtImport(fn);
  }

  provide(key: string, value: TsAppSpec.JsonSerializable): void {
    const s = spec(this);
    if (!s.packageName) {
      throw new Error(
        `Cannot use provide() in a Module created without a package name. ` +
          `Pass the package name to the Module constructor: new Module("@scope/my-module")`
      );
    }
    s.provides.set(key, value);
  }
}

// --- Module (public: packageName required) ---

export class Module extends AppDeclBuilder {
  constructor(packageName: string) {
    super(packageName);
  }
}
