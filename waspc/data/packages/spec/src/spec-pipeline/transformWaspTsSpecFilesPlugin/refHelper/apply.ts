import type { RolldownMagicString } from "rolldown";
import {
  INTERNAL_MAKE_REF_HELPER_IMPORT_NAME,
  INTERNAL_MAKE_REF_HELPER_IMPORT_SOURCE,
} from "../util.js";
import { Plan } from "./plan.js";

export function applyTransformRefHelperPlan_mutate(
  magicString: RolldownMagicString,
  {
    refHelperLocalNames,
    removals,
    safeInternalHelperName,
    safeRefHelperName,
  }: Plan,
): void {
  for (const removal of removals) {
    magicString.remove(removal.start, removal.end);
  }

  // Create the ref helper using the internal helper.
  magicString.prepend(
    [
      `import { ${INTERNAL_MAKE_REF_HELPER_IMPORT_NAME} as ${safeInternalHelperName} } from ${JSON.stringify(INTERNAL_MAKE_REF_HELPER_IMPORT_SOURCE)};\n`,
      `const ${safeRefHelperName} = ${safeInternalHelperName}(import.meta.url);\n`,
      ``,

      // We alias our just-created ref helper to the original imported name(s),
      // so that the rest of the code can remain unchanged.
      ...refHelperLocalNames.map(
        (localName) => `const ${localName} = ${safeRefHelperName};\n`,
      ),
    ].join(""),
  );
}
