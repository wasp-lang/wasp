import type { RolldownMagicString } from "rolldown";
import { RefObject } from "../../index.js";
import type { PlannedImport, PlannedImportReference } from "./plan.js";

export function applyLowerImportsPlan_mutate(
  magicString: RolldownMagicString,
  plan: PlannedImport[],
): void {
  for (const { removeImport, references } of plan) {
    magicString.remove(removeImport.start, removeImport.end);

    for (const ref of references) {
      // Imports are hoisted, so we always prepend the lowered consts to mimic
      // that, regardless of where the original ref import lived in the source
      // file.
      magicString.prepend(getLoweredImportSource(ref));
    }
  }
}

export function getLoweredImportSource(ref: PlannedImportReference): string {
  switch (ref.kind) {
    case "named":
      return getRefObjectBindingSource(
        ref.refObject.alias ?? ref.refObject.import,
        ref.refObject,
      );

    case "default":
      return getRefObjectBindingSource(
        ref.refObject.importDefault,
        ref.refObject,
      );

    case "namespace":
      return getNamespaceProxySource(ref);
  }
}

function getRefObjectBindingSource(identifier: string, refObject: RefObject) {
  return `const ${identifier} = ${JSON.stringify(refObject)} as const;\n`;
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
  ref: Extract<PlannedImportReference, { kind: "namespace" }>,
): string {
  const from = JSON.stringify(ref.from);
  const aliasPrefix = JSON.stringify(`${ref.alias}_`);

  return `const ${ref.alias} = new Proxy({}, { get: (_t, k) => ({ import: String(k), from: ${from}, alias: ${aliasPrefix} + String(k) } as const) }) as Record<string, { import: string; from: ${from}; alias: string }>;\n`;
}
