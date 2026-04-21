/**
 * This module contains sample data that can be used for testing purposes.
 * In our case the sample data represents TsAppSpec data.
 */

import * as AppSpec from "../../src/appSpec.js";
import { Branded } from "../../src/branded.js";
import { App } from "../../src/spec/publicApi/App.js";
import * as TsAppSpec from "../../src/spec/publicApi/tsAppSpec.js";

export function createApp(scope: ConfigType): {
  appConfigName: string;
  app: App;
} {
  const { name: appName, config: appConfig } = getAppConfig(scope);
  const app = new App(appName, appConfig);

  switch (scope) {
    case "minimal":
      return { appConfigName: appName, app };
    case "full": {
      type NamedDeclMethodNameToNamedConfig = {
        [K in keyof App as Parameters<App[K]> extends [string, object]
          ? K
          : never]: NamedConfig<Parameters<App[K]>[1]>;
      };

      /**
       * We thinks this may be a false positive
       * @see https://www.typescriptlang.org/play/?#code/LAKAxgNghgzjAEAZKBzKA7A9vA3qe8ADgE4BuUAFAC4AWAljAFxKoaYA08AHszFcXXQoAlM1KY6AE1wBffPEnEArmmr0mLNFk4946JQFsARgFNio+OKmzQckKCoBPQifgA5KAZMAVTN+euALy48gDaANLwgvAA1iaOmABmmmwAuswAClDEniZUZjAAPMhamBGpAHyhAAyptqCgYJjofPDQWoJ08MHoJgDuKVgUwgDcDSCJSuhgVHTN8PkwMZiFALLwJlz56JIIcQnJHl6+-i4VFOi5zKuc2SjMRz5+AaGrqRZW0nggBO0YnaFLl5UhQ7sJ4AB6CHwABWZngfAEQngADI9IZTMRunoTKQzPUQEA
       */
      function addNamedDecl<M extends keyof NamedDeclMethodNameToNamedConfig>({
        declName,
        namedConfigs,
      }: {
        declName: M;
        namedConfigs: NamedDeclMethodNameToNamedConfig[M][];
      }): void {
        namedConfigs.forEach(({ name, config }) =>
          // eslint-disable-next-line @typescript-eslint/no-explicit-any
          app[declName](name, config as any),
        );
      }

      addNamedDecl({
        declName: "page",
        namedConfigs: getPageConfigs(),
      });
      addNamedDecl({
        declName: "query",
        namedConfigs: getQueryConfigs(),
      });

      return { appConfigName: appName, app };
    }
    default:
      assertUnreachable(scope);
  }
}

export function getAppConfig(
  scope: "minimal",
): MinimalNamedConfig<TsAppSpec.AppConfig>;
export function getAppConfig(
  scope: "full",
): FullNamedConfig<TsAppSpec.AppConfig>;
export function getAppConfig(
  scope: ConfigType,
): NamedConfig<TsAppSpec.AppConfig>;
export function getAppConfig(
  scope: ConfigType,
): NamedConfig<TsAppSpec.AppConfig> {
  switch (scope) {
    case "minimal":
      return {
        name: "MinimalApp",
        config: {
          title: "Mock App",
          wasp: { version: "^0.16.3" },
        },
      } satisfies MinimalNamedConfig<TsAppSpec.AppConfig>;
    case "full":
      return {
        name: "FullApp",
        config: {
          title: "Mock App",
          wasp: { version: "^0.16.3" },
          head: ['<link rel="icon" href="/favicon.ico" />'],
        },
      } satisfies FullNamedConfig<TsAppSpec.AppConfig>;
    default:
      assertUnreachable(scope);
  }
}

export function getPageConfigs(): NamedConfig<TsAppSpec.PageConfig>[] {
  return CONFIG_TYPES.map(getPageConfig);
}

export function getPageConfig(
  pageType: "minimal",
): MinimalNamedConfig<TsAppSpec.PageConfig>;
export function getPageConfig(
  pageType: "full",
): FullNamedConfig<TsAppSpec.PageConfig>;
export function getPageConfig(
  pageType: ConfigType,
): NamedConfig<TsAppSpec.PageConfig>;
export function getPageConfig(
  pageType: ConfigType,
): NamedConfig<TsAppSpec.PageConfig> {
  const name = `${pageType}-page`;
  switch (pageType) {
    case "minimal":
      return {
        name,
        config: {
          component: getExtImport(pageType, "named"),
        },
      } satisfies MinimalNamedConfig<TsAppSpec.PageConfig>;
    case "full":
      return {
        name,
        config: {
          component: getExtImport(pageType, "named"),
          authRequired: true,
        },
      } satisfies NamedConfig<TsAppSpec.PageConfig>;
    default:
      assertUnreachable(pageType);
  }
}

export function getQueryConfigs(): NamedConfig<TsAppSpec.QueryConfig>[] {
  return CONFIG_TYPES.map(getQueryConfig);
}

export function getQueryConfig(
  scope: "minimal",
): MinimalNamedConfig<TsAppSpec.QueryConfig>;
export function getQueryConfig(
  scope: "full",
): FullNamedConfig<TsAppSpec.QueryConfig>;
export function getQueryConfig(
  scope: ConfigType,
): NamedConfig<TsAppSpec.QueryConfig>;
export function getQueryConfig(
  scope: ConfigType,
): NamedConfig<TsAppSpec.QueryConfig> {
  switch (scope) {
    case "minimal":
      return {
        name: "MinimalQuery",
        config: {
          fn: getExtImport(scope, "named"),
        },
      } satisfies MinimalNamedConfig<TsAppSpec.QueryConfig>;
    case "full":
      return {
        name: "FullQuery",
        config: {
          fn: getExtImport(scope, "named"),
          entities: [getEntity("task")],
          auth: true,
        },
      } satisfies FullNamedConfig<TsAppSpec.QueryConfig>;
    default:
      assertUnreachable(scope);
  }
}

export function getExtImport(
  scope: "minimal",
  importKind: AppSpec.ExtImportKind,
): MinimalConfig<TsAppSpec.ExtImport>;
export function getExtImport(
  scope: "full",
  importKind: AppSpec.ExtImportKind,
): FullConfig<TsAppSpec.ExtImport>;
export function getExtImport(
  scope: ConfigType,
  importKind: AppSpec.ExtImportKind,
): Config<TsAppSpec.ExtImport>;
export function getExtImport(
  scope: ConfigType,
  importKind: AppSpec.ExtImportKind,
): Config<TsAppSpec.ExtImport> {
  const importObject =
    importKind === "default"
      ? { importDefault: "defaultExport" }
      : { import: "namedExport" };

  switch (scope) {
    case "minimal":
      return {
        ...importObject,
        from: "@src/external",
      } satisfies MinimalConfig<TsAppSpec.ExtImport>;
    case "full":
      return {
        ...importObject,
        from: "@src/external",
      } satisfies FullConfig<TsAppSpec.ExtImport>;
    default:
      assertUnreachable(scope);
  }
}

export function getEntity(entity: EntityType) {
  switch (entity) {
    case "task":
      return "Task";
    case "user":
      return "User";
    case "social-user":
      return "SocialUser";
    default:
      assertUnreachable(entity);
  }
}

export function getEntities(scope: ConfigType): string[] {
  switch (scope) {
    case "minimal":
      return [];
    case "full":
      return ENTITY_CONFIG_TYPES.map(getEntity);
    default:
      assertUnreachable(scope);
  }
}

function assertUnreachable(value: never): never {
  throw new Error(`Unhandled case: ${value}`);
}

const CONFIG_TYPES = ["minimal", "full"] as const;
type ConfigType = (typeof CONFIG_TYPES)[number];

const ENTITY_CONFIG_TYPES = ["task", "user", "social-user"] as const;
type EntityType = (typeof ENTITY_CONFIG_TYPES)[number];

type NamedConfig<T> = MinimalNamedConfig<T> | FullNamedConfig<T>;
export type Config<T> = MinimalConfig<T> | FullConfig<T>;

type MinimalNamedConfig<T> = {
  name: string;
  config: MinimalConfig<T>;
};

type FullNamedConfig<T> = {
  name: string;
  config: FullConfig<T>;
};

/**
 * Creates a type containing only the required properties from T recursively.
 *
 * This utility:
 * - Filters out optional properties recurisvely
 * - Stops from unwrapping `Branded` types
 * - Returns `EmptyObject` when no required properties exist
 *
 * @template T - The type to extract required properties from
 *
 * @example
 * ```ts
 * // Given the following type:
 * type Result = MinimalConfig<{
 *   a: Branded<string, "A">;
 *   b: {
 *     c: boolean;
 *     d?: {
 *       e: boolean;
 *     };
 *   };
 *   f: {
 *     g: boolean;
 *     h?: string;
 *   }[];
 * };
 * // The result will be:
 * type Result = {
 *   a: Branded<string, "A">;
 *   b: {
 *     c: boolean;
 *   };
 *   f: {
 *     g: boolean;
 *     h: string;
 *   }[];
 * };
 */
export type MinimalConfig<T> =
  T extends Branded<unknown, unknown>
    ? T
    : T extends Array<infer ArrayItem>
      ? Array<MinimalConfig<ArrayItem>>
      : T extends object
        ? keyof T extends never
          ? EmptyObject
          : MinimalConfigObject<T>
        : T;

type MinimalConfigObject<T> = {
  [K in keyof T as EmptyObject extends Pick<T, K> ? never : K]: MinimalConfig<
    T[K]
  >;
} extends infer Object
  ? Object extends EmptyObject
    ? EmptyObject
    : Object
  : never;

/**
 * Represents an empty object type in TypeScript.
 * @see https://www.totaltypescript.com/the-empty-object-type-in-typescript
 */
type EmptyObject = Record<string, never>;

/**
 * Creates a type with all properties of T required recursively.
 *
 * This utility:
 * - Makes all properties required recursively
 * - Stops from unwrapping branded types
 *
 * @template T - The type to make fully required
 *
 * @example
 * ```ts
 * // Given the following type:
 * type Result = FullConfig<{
 *   a: Branded<string, "A">;
 *   b: {
 *     c: boolean;
 *     d?: {
 *       e: boolean;
 *     };
 *   };
 *   f: {
 *     g: boolean;
 *     h?: string;
 *   }[];
 * };
 * // The result will be:
 * type Result = FullConfig<{
 *   a: Branded<string, "A">;
 *   b: {
 *     c: boolean;
 *     d: {
 *       e: boolean;
 *     };
 *   };
 *   f: {
 *     g: boolean;
 *     h: string;
 *   }[];
 * };
 */
export type FullConfig<T> =
  T extends Branded<unknown, unknown>
    ? T
    : T extends Array<infer ArrayItem>
      ? Array<FullConfig<ArrayItem>>
      : T extends object
        ? FullConfigObject<T>
        : T;

type FullConfigObject<T> = {
  [K in keyof T]-?: FullConfig<T[K]>;
};
