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
      throw new Error("Namespace imports are not supported for ref imports.");
  }
}

function getRefObjectBindingSource(identifier: string, refObject: RefObject) {
  return `const ${identifier} = ${JSON.stringify(refObject)} as const;\n`;
}
