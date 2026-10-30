import { WaspSpecUserError } from "../../../spec/waspSpecUserError.js";
import { PUBLIC_REF_HELPER_IMPORT_SOURCE } from "../util.js";
export function assertCanTransformRefHelper(ast) {
    const hasRefHelperExports = ast.body.some(isRefHelperExportDeclaration);
    if (hasRefHelperExports) {
        throw new WaspSpecUserError(`Re-exporting from the ${PUBLIC_REF_HELPER_IMPORT_SOURCE} package is not supported. First import it and then re-export it if needed.`);
    }
}
function isRefHelperExportDeclaration(node) {
    return ((node.type === "ExportNamedDeclaration" ||
        node.type === "ExportAllDeclaration") &&
        node.source?.value === PUBLIC_REF_HELPER_IMPORT_SOURCE);
}
