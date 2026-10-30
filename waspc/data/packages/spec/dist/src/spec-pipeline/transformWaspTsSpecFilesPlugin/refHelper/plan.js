import { getStringValue, getTopLevelBindings, INTERNAL_MAKE_REF_HELPER_IMPORT_NAME, makeSafeName, PUBLIC_REF_HELPER_IMPORT_NAME, PUBLIC_REF_HELPER_IMPORT_SOURCE, } from "../util.js";
export function planTransformRefHelper(ast) {
    const refHelperImportDeclarations = ast.body.filter(isRefHelperImportDeclaration);
    if (refHelperImportDeclarations.length === 0) {
        return null;
    }
    const removals = refHelperImportDeclarations.flatMap(planRefSpecifierRemovals);
    const refHelperLocalNames = refHelperImportDeclarations.flatMap((declaration) => declaration.specifiers
        .filter(isRefHelperImportSpecifier)
        .map((specifier) => specifier.local.name));
    const scope = getTopLevelBindings(ast);
    const safeMakeRefHelperName = makeSafeName(INTERNAL_MAKE_REF_HELPER_IMPORT_NAME, scope);
    return {
        refHelperLocalNames,
        removals,
        safeMakeRefHelperName,
    };
}
function isRefHelperImportDeclaration(node) {
    return (node.type === "ImportDeclaration" &&
        node.importKind === "value" &&
        getStringValue(node.source) === PUBLIC_REF_HELPER_IMPORT_SOURCE);
}
// We want to remove all `ref` specifiers, with two caveats:
// - If `ref` is the only specifier in its import declaration, we remove the
//   whole declaration.
// - If there are multiple specifiers, we need to extend the range a bit to also
//   remove the comma next to it, as a leftover comma would cause a SyntaxError.
function planRefSpecifierRemovals(importDeclaration) {
    const specifiersCount = importDeclaration.specifiers.length;
    const specifiersToRemove = importDeclaration.specifiers
        .map((node, position) => ({ node, position }))
        .filter(({ node }) => isRefHelperImportSpecifier(node));
    if (specifiersToRemove.length === 0)
        return [];
    if (specifiersToRemove.length === specifiersCount)
        return [importDeclaration];
    return specifiersToRemove.map(({ node: currentSpecifier, position }) => {
        const isFirstSpecifier = position === 0;
        if (isFirstSpecifier) {
            const nextSpecifier = importDeclaration.specifiers[position + 1];
            return {
                start: currentSpecifier.start,
                end: nextSpecifier?.start ?? currentSpecifier.end,
            };
        }
        else {
            const previousSpecifier = importDeclaration.specifiers[position - 1];
            return {
                start: previousSpecifier?.end ?? currentSpecifier.start,
                end: currentSpecifier.end,
            };
        }
    });
}
function isRefHelperImportSpecifier(node) {
    return (node.type === "ImportSpecifier" &&
        node.importKind === "value" &&
        getStringValue(node.imported) === PUBLIC_REF_HELPER_IMPORT_NAME);
}
