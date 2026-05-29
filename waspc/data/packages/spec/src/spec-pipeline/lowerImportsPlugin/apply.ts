import type { RolldownMagicString } from "rolldown";
import type { PlannedImport } from "./plan.js";

export function applyLowerImportsPlan(
  magicString: RolldownMagicString,
  plan: PlannedImport[],
): void {
  for (const plannedImport of plan) {
    magicString.remove(plannedImport.start, plannedImport.end);

    for (const binding of plannedImport.bindings) {
      // Imports are hoisted, so we prepend the lowered consts to mimic that
      // regardless of where the original ref import lived in the source file.
      magicString.prepend(
        `const ${binding.alias} = ${JSON.stringify({
          import: binding.import,
          alias: binding.alias,
          from: plannedImport.from,
        })} as const;\n`,
      );
    }
  }
}
