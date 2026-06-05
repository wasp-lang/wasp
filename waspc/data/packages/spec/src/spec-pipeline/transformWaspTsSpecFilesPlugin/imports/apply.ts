import type { RefObjectDescriptor } from "@wasp.sh/spec";
import type { RolldownMagicString } from "rolldown";
import {
  PUBLIC_REF_HELPER_IMPORT_NAME,
  PUBLIC_REF_HELPER_IMPORT_SOURCE,
} from "../util.js";
import type { Plan, RefImportReference } from "./plan.js";

export function applyTransformImportsPlan_mutate(
  magicString: RolldownMagicString,
  { refImports, safeRefHelperName }: Plan,
): void {
  for (const { removeImport } of refImports) {
    magicString.remove(removeImport.start, removeImport.end);
  }

  magicString.prepend(
    [
      // Add the `ref` helper import
      `import { ${PUBLIC_REF_HELPER_IMPORT_NAME} as ${safeRefHelperName} } from ${JSON.stringify(
        PUBLIC_REF_HELPER_IMPORT_SOURCE,
      )};\n`,

      // Convert each original ref import to a `const` declaration that uses the
      // `ref` helper.
      // In ES Modules, imports are always hoisted, so we add these declarations
      // at the top of the file to preserve the original semantics.
      ...refImports.flatMap(({ references }) =>
        references.map((ref) =>
          getLoweredImportSource(ref, {
            refHelperName: safeRefHelperName,
          }),
        ),
      ),
    ].join(""),
  );
}

function getLoweredImportSource(
  ref: RefImportReference,
  ctx: { refHelperName: string },
): string {
  switch (ref.kind) {
    case "named":
      return getRefObjectBindingSource(
        ref.refObject.alias ?? ref.refObject.import,
        ref.refObject,
        ctx,
      );

    case "default":
      return getRefObjectBindingSource(
        ref.refObject.importDefault,
        ref.refObject,
        ctx,
      );

    case "namespace":
      return getNamespaceProxySource(ref, ctx);
  }
}

function getRefObjectBindingSource(
  identifier: string,
  descriptor: RefObjectDescriptor,
  { refHelperName }: { refHelperName: string },
) {
  return `const ${identifier} = ${refHelperName}(${JSON.stringify(
    descriptor satisfies RefObjectDescriptor,
  )});\n`;
}

/**
 * Namespace imports are lowered to a Proxy so `import * as ops` can support any
 * `ops.anything` access by returning `{ import: "anything", from: ... }`. This
 * avoids enumerating every place where `ops` is used.
 *
 * The generated alias is prefixed with the local name (e.g. `ops.archive`
 * becomes `ops_archive`) so multiple namespace imports don't collide.
 */
function getNamespaceProxySource(
  ref: Extract<RefImportReference, { kind: "namespace" }>,
  { refHelperName }: { refHelperName: string },
): string {
  const from = JSON.stringify(ref.from);
  const aliasPrefix = JSON.stringify(`${ref.alias}_`);

  return `const ${ref.alias} = new Proxy({}, { get: (_t, k) => ${refHelperName}({ import: String(k), from: ${from}, alias: ${aliasPrefix} + String(k) }) }) as Record<string, ReturnType<typeof ${refHelperName}>>;\n`;
}
