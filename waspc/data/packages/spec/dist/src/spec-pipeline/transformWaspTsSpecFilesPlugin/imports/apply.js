import { WaspSpecUserError } from "../../../spec/waspSpecUserError.js";
import { buildImportStatement, PUBLIC_REF_HELPER_IMPORT_NAME, PUBLIC_REF_HELPER_IMPORT_SOURCE, } from "../util.js";
export function applyTransformImportsPlan_mutate(magicString, { refImports, safeRefHelperName }) {
    for (const { removeImport } of refImports) {
        magicString.remove(removeImport.start, removeImport.end);
    }
    magicString.prepend([
        // Add the `ref` helper import
        buildImportStatement([[PUBLIC_REF_HELPER_IMPORT_NAME, safeRefHelperName]], PUBLIC_REF_HELPER_IMPORT_SOURCE),
        // Convert each original ref import to a `const` declaration that uses the
        // `ref` helper.
        // In ES Modules, imports are always hoisted, so we add these declarations
        // at the top of the file to preserve the original semantics.
        ...refImports.flatMap(({ references }) => references.map((ref) => getLoweredImportSource(ref, {
            refHelperName: safeRefHelperName,
        }))),
    ].join(""));
}
function getLoweredImportSource(ref, ctx) {
    switch (ref.kind) {
        case "named":
            return getRefObjectBindingSource(ref.refObject.alias ?? ref.refObject.import, ref.refObject, ctx);
        case "default":
            return getRefObjectBindingSource(ref.refObject.importDefault, ref.refObject, ctx);
        case "namespace":
            throw new WaspSpecUserError([
                "Namespace imports are not supported for reference imports.",
                `Replace \`import * as ${ref.alias} from "${ref.from}" with { type: "ref" }\` with a named or default reference import.`,
            ].join("\n"));
    }
}
function getRefObjectBindingSource(identifier, descriptor, { refHelperName }) {
    return `const ${identifier} = ${refHelperName}(${JSON.stringify(descriptor)});\n`;
}
