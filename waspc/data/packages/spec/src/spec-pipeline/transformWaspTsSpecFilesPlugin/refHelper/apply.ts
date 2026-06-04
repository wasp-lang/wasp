import type { RolldownMagicString } from "rolldown";
import {
  getStringValue,
  INTERNAL_MAKE_REF_HELPER_IMPORT_NAME,
  INTERNAL_MAKE_REF_HELPER_IMPORT_SOURCE,
} from "../util.js";
import { Plan } from "./plan.js";

export function applyTransformRefHelperPlan_mutate(
  magicString: RolldownMagicString,
  { refHelperImports, safeInternalHelperName, safeRefHelperName }: Plan,
): void {
  if (refHelperImports.length === 0) {
    return;
  }

  for (const importSpecifier of refHelperImports) {
    magicString.remove(importSpecifier.start, importSpecifier.end);
  }

  // Create the ref helper using the internal helper.
  magicString.prepend(
    [
      `import { ${INTERNAL_MAKE_REF_HELPER_IMPORT_NAME} as ${safeInternalHelperName} } from ${JSON.stringify(INTERNAL_MAKE_REF_HELPER_IMPORT_SOURCE)};\n`,
      `const ${safeRefHelperName} = ${safeInternalHelperName}(import.meta.url);\n`,
      ``,

      // We alias our just-created ref helper to the original imported name(s),
      // so that the rest of the code can remain unchanged.
      ...refHelperImports.map(
        (importSpecifier) =>
          `const ${getStringValue(importSpecifier.local)} = ${safeRefHelperName};\n`,
      ),
    ].join(""),
  );
}
