import type { RefObjectDescriptor } from "@wasp.sh/spec";
import type { RolldownMagicString } from "rolldown";
import { SpecUserError } from "../../../spec/specUserError.js";
import {
  buildImportStatement,
  PUBLIC_REF_HELPER_IMPORT_NAME,
  PUBLIC_REF_HELPER_IMPORT_SOURCE,
} from "../util.js";
import type { Plan, RefImportReference } from "./plan.js";

export type TransformRefImportOptions = {
  mapRefObjectDescriptor?: (
    descriptor: RefObjectDescriptor,
  ) => RefObjectDescriptor;
  getRefHelperPrelude?: (plan: Plan) => string;
};

export function applyTransformImportsPlan_mutate(
  magicString: RolldownMagicString,
  plan: Plan,
  options: TransformRefImportOptions = {},
): void {
  const { refImports, safeRefHelperName } = plan;

  for (const { removeImport } of refImports) {
    magicString.remove(removeImport.start, removeImport.end);
  }

  magicString.prepend(
    [
      // Add the `ref` helper import, or a custom prelude that defines it.
      options.getRefHelperPrelude?.(plan) ??
        buildImportStatement(
          [[PUBLIC_REF_HELPER_IMPORT_NAME, safeRefHelperName]],
          PUBLIC_REF_HELPER_IMPORT_SOURCE,
        ),

      // Convert each original ref import to a `const` declaration that uses the
      // `ref` helper. In ES Modules, imports are always hoisted, so we add these
      // declarations at the top of the file to preserve the original semantics.
      ...refImports.flatMap(({ references }) =>
        references.map((ref) =>
          getLoweredImportSource(ref, {
            refHelperName: safeRefHelperName,
            mapRefObjectDescriptor: options.mapRefObjectDescriptor,
          }),
        ),
      ),
    ].join(""),
  );
}

function getLoweredImportSource(
  ref: RefImportReference,
  ctx: {
    refHelperName: string;
    mapRefObjectDescriptor?: (
      descriptor: RefObjectDescriptor,
    ) => RefObjectDescriptor;
  },
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
      throw new SpecUserError(
        [
          "Namespace imports are not supported for reference imports.",
          `Replace \`import * as ${ref.alias} from "${ref.from}" with { type: "ref" }\` with a named or default reference import.`,
        ].join("\n"),
      );
  }
}

function getRefObjectBindingSource(
  identifier: string,
  descriptor: RefObjectDescriptor,
  {
    refHelperName,
    mapRefObjectDescriptor,
  }: {
    refHelperName: string;
    mapRefObjectDescriptor?: (
      descriptor: RefObjectDescriptor,
    ) => RefObjectDescriptor;
  },
) {
  const mappedDescriptor = mapRefObjectDescriptor?.(descriptor) ?? descriptor;

  return `const ${identifier} = ${refHelperName}(${JSON.stringify(
    mappedDescriptor satisfies RefObjectDescriptor,
  )});\n`;
}
