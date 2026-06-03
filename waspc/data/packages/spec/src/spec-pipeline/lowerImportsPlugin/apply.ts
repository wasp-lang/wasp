import type { RolldownMagicString } from "rolldown";
import { RefObject } from "../../index.js";
import { SpecUserError } from "../../spec/specUserError.js";
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
      throw new SpecUserError(
        [
          "Namespace imports are not supported for reference imports.",
          `Replace \`import * as ${ref.alias} from "${ref.from}" with { type: "ref" }\` with a named or default reference imports.`,
        ].join("\n"),
      );
  }
}

function getRefObjectBindingSource(identifier: string, refObject: RefObject) {
  return `const ${identifier} = ${JSON.stringify(refObject)} as const;\n`;
}
