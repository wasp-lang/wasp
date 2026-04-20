/**
 * Sample data for testing the spec pipeline.
 * Modeled on __tests__/legacy/testFixtures.ts; scoped to what the spec
 * surface currently supports (`page`, `query`).
 */

import * as AppSpec from "../../src/appSpec.js";
import { Branded } from "../../src/branded.js";
import { page, query } from "../../src/spec/publicApi/index.js";
import * as TsAppSpec from "../../src/spec/publicApi/tsAppSpec.js";

const CONFIG_TYPES = ["minimal", "full"] as const;
type ConfigType = (typeof CONFIG_TYPES)[number];

export function getPage(scope: "minimal"): MinimalConfig<TsAppSpec.Page>;
export function getPage(scope: "full"): FullConfig<TsAppSpec.Page>;
export function getPage(scope: ConfigType): Config<TsAppSpec.Page>;
export function getPage(scope: ConfigType): Config<TsAppSpec.Page> {
  switch (scope) {
    case "minimal":
      return page(getExtImport("minimal", "named"));
    case "full":
      return page(getExtImport("full", "named"), { authRequired: true });
    default:
      assertUnreachable(scope);
  }
}

export function getQuery(scope: "minimal"): MinimalConfig<TsAppSpec.Query>;
export function getQuery(scope: "full"): FullConfig<TsAppSpec.Query>;
export function getQuery(scope: ConfigType): Config<TsAppSpec.Query>;
export function getQuery(scope: ConfigType): Config<TsAppSpec.Query> {
  switch (scope) {
    case "minimal":
      return query(getExtImport("minimal", "named"));
    case "full":
      return query(getExtImport("full", "named"), {
        entities: ["Task"],
        auth: true,
      });
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
  switch (importKind) {
    case "named": {
      return {
        import: "namedExport",
        ...(scope == "full" && { alias: "namedAlias" }),
        from: "@src/external",
      };
    }
    case "default":
      return { importDefault: "defaultExport", from: "@src/external" };
    default:
      assertUnreachable(importKind);
  }
}

function assertUnreachable(value: never): never {
  throw new Error(`Unhandled case: ${value}`);
}

export type Config<T> = MinimalConfig<T> | FullConfig<T>;

/**
 * Recursively strips optional properties from T.
 * - Branded types are passed through (don't recurse into the brand).
 * - Arrays recurse into element type.
 * - Objects keep only required keys (collapse to EmptyObject when none remain).
 * - Primitives pass through.
 */
export type MinimalConfig<T> =
  T extends Branded<unknown, unknown>
    ? T
    : T extends Array<infer Item>
      ? Array<MinimalConfig<Item>>
      : T extends object
        ? keyof T extends never
          ? EmptyObject
          : MinimalConfigObject<T>
        : T;

type MinimalConfigObject<T> = {
  [K in keyof T as EmptyObject extends Pick<T, K> ? never : K]: MinimalConfig<
    T[K]
  >;
} extends infer Result
  ? Result extends EmptyObject
    ? EmptyObject
    : Result
  : never;

type EmptyObject = Record<string, never>;

/**
 * Recursively makes every property of T required.
 * - Branded types are passed through (don't recurse into the brand).
 * - Arrays recurse into element type.
 * - Objects strip every `?` and recurse.
 * - Primitives pass through.
 */
export type FullConfig<T> =
  T extends Branded<unknown, unknown>
    ? T
    : T extends Array<infer Item>
      ? Array<FullConfig<Item>>
      : T extends object
        ? FullConfigObject<T>
        : T;

type FullConfigObject<T> = {
  [K in keyof T]-?: FullConfig<T[K]>;
};
