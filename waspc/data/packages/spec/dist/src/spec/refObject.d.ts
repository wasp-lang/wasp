import type * as AppSpec from "../appSpec.js";
import type { Branded } from "../branded.js";
/**
 * A reference to code in your app's `src` directory.
 *
 * @category References
 */
export type RefObject = Branded<RefObjectDescriptor & {
    kind: "refObject";
}, "RefObject">;
/**
 * Input accepted by {@link ref}.
 *
 * @category References
 */
export type RefObjectDescriptor = NamedRefObjectDescriptor | DefaultRefObjectDescriptor;
/**
 * Named import reference, equivalent to
 * `import { SomeValue } from "./src/someModule" with { type: "ref" }`.
 *
 * @category References
 */
export interface NamedRefObjectDescriptor {
    /** Exported name to import. */
    import: string;
    /**
     * Optional local alias.
     *
     * Alias takes precedence over the `import` field when
     * Wasp Spec dervies some {@link WaspSpec.SpecElement} name.
     */
    alias?: string;
    /** Module path, relative to the `*.wasp.ts` file using it. */
    from: string;
}
/**
 * Default import reference, equivalent to
 * `import SomeValue from "./src/someModule" with { type: "ref" }`.
 *
 * @category References
 */
export interface DefaultRefObjectDescriptor {
    /** Local name for the default import. */
    importDefault: string;
    /** Module path, relative to the `*.wasp.ts` file using it. */
    from: string;
}
/**
 * Creates a fallback reference object for a value from your app.
 *
 * {@include ./publicApi/referenceImports.md}
 *
 * Reference imports are preferred because editors can follow and rename real
 * imports.
 *
 * The import path must be relative to the `*.wasp.ts` file where it is used
 * and resolve inside the app's `src/` directory. Absolute
 * paths are not supported.
 *
 * @category References
 *
 * @example
 * ```ts
 * import { page, ref } from "@wasp/spec"
 *
 * const MainPage = ref({
 *   importDefault: "MainPage",
 *   from: "./src/MainPage",
 * })
 *
 * export const mainPage = page(MainPage)
 * ```
 */
export declare function ref(_descriptor: RefObjectDescriptor): RefObject;
/**
 * Creates a `ref` helper bound to the user's `.wasp.ts` file.
 *
 * Ref objects need the current spec file location to resolve relative paths,
 * but `ref` itself can't use `import.meta.url` because it would point to this
 * helper module. `_waspMakeRef(sourceFilePath)` lets each `.wasp.ts` file
 * create a local `ref` that carries its own source file path.
 *
 * @internal
 */
export declare function _waspMakeRef(sourceFilePath: string): (descriptor: RefObjectDescriptor) => SourceAwareRefObject;
export declare function mapRefObject(refObject: unknown, { projectRootDir }: {
    projectRootDir: string;
}): AppSpec.ExtImport;
export declare function getRefObjectDeclarationName(refObject: unknown): string;
type RefObjectSource = {
    sourceFilePath: string;
};
type SourceAwareRefObject = RefObject & RefObjectSource;
export {};
//# sourceMappingURL=refObject.d.ts.map