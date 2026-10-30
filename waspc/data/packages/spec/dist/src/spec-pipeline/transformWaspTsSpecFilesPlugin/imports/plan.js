import { WaspSpecUserError } from "../../../spec/waspSpecUserError.js";
import { getStringValue, getTopLevelBindings, makeSafeName, PUBLIC_REF_HELPER_IMPORT_NAME, } from "../util.js";
export function planTransformImports(ast) {
    const refImports = findRefImports(ast);
    if (refImports.length === 0) {
        return null;
    }
    const scope = getTopLevelBindings(ast);
    const safeRefHelperName = makeSafeName(PUBLIC_REF_HELPER_IMPORT_NAME, scope);
    return { refImports, safeRefHelperName };
}
function findRefImports(ast) {
    return ast.body.filter(isRefImportDeclaration).map((node) => {
        const importSource = getStringValue(node.source);
        if (node.specifiers.length === 0) {
            throw new WaspSpecUserError(`Ref import from ${JSON.stringify(importSource)} must import at least one binding.`);
        }
        return {
            references: node.specifiers.map((specifier) => makeRefObject(importSource, specifier)),
            removeImport: { start: node.start, end: node.end },
        };
    });
}
function isRefImportDeclaration(node) {
    return (node.type === "ImportDeclaration" &&
        node.attributes.some((attr) => getStringValue(attr.key) === "type" &&
            getStringValue(attr.value) === "ref"));
}
function makeRefObject(importSource, specifier) {
    switch (specifier.type) {
        case "ImportSpecifier":
            return {
                kind: "named",
                refObject: {
                    import: getStringValue(specifier.imported),
                    alias: specifier.local.name,
                    from: importSource,
                },
            };
        case "ImportDefaultSpecifier":
            return {
                kind: "default",
                refObject: {
                    importDefault: specifier.local.name,
                    from: importSource,
                },
            };
        case "ImportNamespaceSpecifier":
            return {
                kind: "namespace",
                alias: specifier.local.name,
                from: importSource,
            };
        default:
            return specifier;
    }
}
