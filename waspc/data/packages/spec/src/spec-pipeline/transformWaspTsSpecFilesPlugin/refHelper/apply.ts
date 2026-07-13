import type { RolldownMagicString } from "rolldown";
import type { RefOrigin } from "../../../spec/refObject.js";
import {
  buildImportStatement,
  INTERNAL_MAKE_REF_HELPER_IMPORT_NAME,
  INTERNAL_MAKE_REF_HELPER_IMPORT_SOURCE,
} from "../util.js";
import { Plan } from "./plan.js";

export function applyTransformRefHelperPlan_mutate(
  origin: RefOrigin,
  magicString: RolldownMagicString,
  {
    refHelperLocalNames,
    removals,
    safeMakeRefHelperName: safeMakeRefHelperName,
  }: Plan,
): void {
  for (const removal of removals) {
    magicString.remove(removal.start, removal.end);
  }

  if (refHelperLocalNames.length === 0) {
    return;
  }

  // We'll use the first local name for the ref helper as the main one, and create
  // aliases for the rest if needed. This way we minimize the number of new
  // variables we introduce.
  const [firstRefHelperLocalName, ...extraRefHelperAliases] =
    refHelperLocalNames;

  magicString.prepend(
    [
      buildImportStatement(
        [[INTERNAL_MAKE_REF_HELPER_IMPORT_NAME, safeMakeRefHelperName]],
        INTERNAL_MAKE_REF_HELPER_IMPORT_SOURCE,
      ),
      `const ${firstRefHelperLocalName} = ${safeMakeRefHelperName}(${JSON.stringify(origin)});\n`,
      ...extraRefHelperAliases.map(
        (localName) => `const ${localName} = ${firstRefHelperLocalName};\n`,
      ),
    ].join(""),
  );
}
