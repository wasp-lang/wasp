import { INTERNAL_MAKE_REF_HELPER_IMPORT_SOURCE, PUBLIC_REF_HELPER_IMPORT_SOURCE, } from "./transformWaspTsSpecFilesPlugin/util.js";
/**
 * Resolves imports of this package (`@wasp/spec`) in the user's spec files to
 * the copy of the package that is running the analysis.
 *
 * Users depend on `@wasp/wasp`, which depends on `@wasp/spec`. Package
 * managers that don't hoist (pnpm) then don't expose `@wasp/spec` at the
 * project root, so the bundler couldn't resolve it from `main.wasp.ts`. Using
 * our own copy also keeps a single copy at runtime, which `instanceof` checks
 * on the errors it throws rely on.
 *
 * The modules are marked external, so the user's spec file imports them at
 * runtime instead of bundling a second copy. The IDs are `file:` URLs, not
 * paths. The bundler writes them verbatim into the output's import statement,
 * and Node accepts a `file:` URL on every platform, while it rejects a
 * Windows path such as `C:\...` as an unsupported URL scheme.
 */
export function waspSpecAliasPlugin() {
    const specEntryPoints = {
        [PUBLIC_REF_HELPER_IMPORT_SOURCE]: getSpecDistFileUrl("index.js"),
        [INTERNAL_MAKE_REF_HELPER_IMPORT_SOURCE]: getSpecDistFileUrl("internal.js"),
    };
    return {
        name: "wasp-spec-alias",
        resolveId(source) {
            const specEntryPoint = specEntryPoints[source];
            return specEntryPoint === undefined
                ? null
                : { id: specEntryPoint, external: true };
        },
    };
}
// This file lives in `dist/src/spec-pipeline/`, the entry points in `dist/src/`.
function getSpecDistFileUrl(fileName) {
    return new URL(`../${fileName}`, import.meta.url).href;
}
