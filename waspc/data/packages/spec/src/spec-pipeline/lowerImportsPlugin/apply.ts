import type { RolldownMagicString } from "rolldown";
import type { PlannedImport, PlannedImportBinding } from "./plan.js";

export function applyLowerImportsPlan(
  magicString: RolldownMagicString,
  plan: PlannedImport[],
): void {
  for (const plannedImport of plan) {
    magicString.remove(plannedImport.start, plannedImport.end);

    for (const binding of plannedImport.bindings) {
      // Imports are hoisted, so we prepend the lowered consts to mimic that
      // regardless of where the original ref import lived in the source file.
      magicString.prepend(getLoweredImportSource(binding, plannedImport.from));
    }
  }
}

/**
 * Renders a single lowered ref import as a `const` declaration whose value is
 * a RefObject literal. Default imports become a `DefaultRefObject`
 * (`importDefault`), named imports a `NamedRefObject` (`import`, plus `alias`
 * only when the local name differs from the imported one).
 */
export function getLoweredImportSource(
  binding: PlannedImportBinding,
  from: string,
): string {
  return `const ${binding.alias} = ${JSON.stringify(toRefObject(binding, from))} as const;\n`;
}

function toRefObject(
  binding: PlannedImportBinding,
  from: string,
): Record<string, string> {
  if (binding.import === "default") {
    return { importDefault: binding.alias, from };
  }

  if (binding.import === binding.alias) {
    return { import: binding.import, from };
  }

  return { import: binding.import, alias: binding.alias, from };
}
