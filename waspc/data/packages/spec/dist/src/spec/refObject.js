import { normalizeRefObjectPath } from "./refObjectPath.js";
import { WaspSpecUserError } from "./waspSpecUserError.js";
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
export function ref(_descriptor) {
    throw new Error("Missing Wasp transformation. The `.wasp.ts` files are not directly executable, use the Wasp CLI.");
}
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
export function _waspMakeRef(sourceFilePath) {
    return (descriptor) => {
        const refObject = {
            ...descriptor,
            kind: "refObject",
        };
        return { ...refObject, sourceFilePath };
    };
}
export function mapRefObject(refObject, { projectRootDir }) {
    if (isNamedRefObject(refObject)) {
        return {
            kind: "named",
            name: refObject.import,
            path: mapRefObjectPath(refObject, { projectRootDir }),
            alias: refObject.alias,
        };
    }
    else if (isDefaultRefObject(refObject)) {
        return {
            kind: "default",
            name: refObject.importDefault,
            path: mapRefObjectPath(refObject, { projectRootDir }),
        };
    }
    else {
        throw new WaspSpecUserError("Got an import in the Wasp file that we couldn't process: " +
            JSON.stringify(refObject) +
            '\nYou either used a value imported without `with { type: "ref" }` or didn\'t write the ref object correctly.');
    }
}
export function getRefObjectDeclarationName(refObject) {
    if (isNamedRefObject(refObject)) {
        return refObject.alias ?? refObject.import;
    }
    if (isDefaultRefObject(refObject)) {
        return refObject.importDefault;
    }
    throw new WaspSpecUserError("Got an import in the Wasp file that we couldn't process: " +
        JSON.stringify(refObject));
}
function mapRefObjectPath(refObject, { projectRootDir }) {
    if (!hasSourceFilePath(refObject)) {
        throw new WaspSpecUserError(`Relative ref path ${JSON.stringify(refObject.from)} is missing source file information. Use \`ref(...)\` in a \`*.wasp.ts\` file.`);
    }
    return normalizeRefObjectPath({
        importPath: refObject.from,
        importingFilePath: refObject.sourceFilePath,
        projectRootDir,
    });
}
function hasSourceFilePath(value) {
    return (isObject(value) &&
        "sourceFilePath" in value &&
        typeof value.sourceFilePath === "string");
}
function isNamedRefObject(value) {
    return (isObject(value) &&
        typeof value.import === "string" &&
        typeof value.from === "string" &&
        hasValidAlias(value) &&
        hasRefObjectMarker(value));
}
function isDefaultRefObject(value) {
    return (isObject(value) &&
        typeof value.importDefault === "string" &&
        typeof value.from === "string" &&
        hasRefObjectMarker(value));
}
function hasRefObjectMarker(value) {
    return value.kind === "refObject";
}
function hasValidAlias(value) {
    return value.alias === undefined || typeof value.alias === "string";
}
function isObject(value) {
    return Object.prototype.toString.call(value) === "[object Object]";
}
