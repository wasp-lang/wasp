/**
 * waspImport — a typed dynamic import for .wasp.ts files that preserves
 * `with { type: "ref" }` lowering across module boundaries.
 *
 * Usage (in a library .wasp.ts helper):
 *
 *   const { default: routes } = await waspImport("./routes.wasp.ts");
 *
 * This is the only safe way for userland libraries to dynamically import
 * other .wasp.ts files. A plain `import()` does NOT trigger ref-lowering.
 *
 * @see https://github.com/wasp-lang/wasp/issues/4358
 */

/** The shape of a lowered spec ref, produced by `with { type: "ref" }` imports. */
export type WaspRef<T = unknown> = {
  readonly __waspRef: true;
  readonly import: string;
  readonly from: string;
  // Carry the TypeScript type so callers stay type-safe, even though at
  // runtime this is just a plain serializable object.
  readonly _phantom?: T;
};

/**
 * Dynamically imports a `.wasp.ts` module, making sure all
 * `with { type: "ref" }` imports inside that module are correctly lowered
 * before the module's exports are returned.
 *
 * The generic `T` should be the type of the module's default export.
 *
 * @example
 * // In a library helper:
 * const { default: authRoutes } = await waspImport<typeof import("./auth.wasp.ts")>(
 *   "./auth.wasp.ts"
 * );
 */
export async function waspImport<T = Record<string, unknown>>(
  specPath: string,
): Promise<T> {
  // The Wasp bundler/compiler recognises calls to `waspImport()` from
  // `@wasp.sh/spec` and applies the same ref-lowering transform it uses
  // for static `with { type: "ref" }` imports, before handing the module
  // off to the JS runtime.
  //
  // At runtime (when running outside the Wasp compiler, e.g. in tests) we
  // fall back to a plain dynamic import so code remains executable.
  return import(/* @wasp-spec-import */ specPath) as Promise<T>;
}
