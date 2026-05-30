import type { RefImportDescriptor } from "../../spec/refImport.js";
import type { ImportLoweringPlan } from "./planImportLowering/index.js";
import { planImportLowering } from "./planImportLowering/index.js";
import type {
  LoweredImportBinding,
  NamespaceImportBinding,
} from "./planImportLowering/loweredImportBindings.js";
import { applyEdits, type Edit } from "./sourceEdits.js";

/**
 * Given source code, finds supported ref import statements and replaces them
 * with inline ref(...) consts. We call this lowering imports.
 */
export function lowerRefImports({
  sourceText,
  sourcePath,
  refName,
}: {
  sourceText: string;
  sourcePath: string;
  refName: string;
}): string {
  const plan = planImportLowering({
    sourceText,
    sourcePath,
  });

  return applyEdits(sourceText, getImportLoweringEdits(plan, refName));
}

function getImportLoweringEdits(
  plan: ImportLoweringPlan,
  refName: string,
): Edit[] {
  return plan.replacements.map((replacement) => ({
    start: replacement.start,
    end: replacement.end,
    text: getImportReplacementSource(replacement.bindings, refName),
  }));
}

function getImportReplacementSource(
  bindings: LoweredImportBinding[],
  refName: string,
): string {
  return bindings
    .map((binding) => getLoweredImportBindingSource(binding, refName))
    .join("\n");
}

function getLoweredImportBindingSource(
  binding: LoweredImportBinding,
  refName: string,
): string {
  switch (binding.kind) {
    case "refImport":
      return `const ${binding.localName} = ${refName}(${getRefImportDescriptorObjectLiteralSource(binding.descriptor)});`;
    case "namespace":
      return getNamespaceImportProxySource(binding, refName);
  }
}

/**
 * Namespace imports are lowered to a Proxy so `import * as ops` can support any
 * `ops.anything` access by returning `{ import: "anything", from: ... }`. This
 * avoids enumerating every place where `ops` is used.
 */
function getNamespaceImportProxySource(
  binding: NamespaceImportBinding,
  refName: string,
): string {
  const from = JSON.stringify(binding.from);
  const aliasPrefix = JSON.stringify(binding.aliasPrefix);

  return `const ${binding.localName} = new Proxy({}, { get: (_t, k) => ${refName}({ import: String(k), from: ${from}, alias: ${aliasPrefix} + String(k) }) }) as Record<string, ReturnType<typeof ${refName}>>;`;
}

function getRefImportDescriptorObjectLiteralSource(
  descriptor: RefImportDescriptor,
): string {
  if ("import" in descriptor) {
    const fields: Field[] = [
      ["import", descriptor.import],
      ["from", descriptor.from],
      ["alias", descriptor.alias],
    ];

    return `{ ${getObjectFieldsSource(fields)} }`;
  } else {
    const fields: Field[] = [
      ["importDefault", descriptor.importDefault],
      ["from", descriptor.from],
    ];

    return `{ ${getObjectFieldsSource(fields)} }`;
  }
}

/**
 * [["key", "value"], ["key2", "value2"]] => "key: "value", key2: "value2""
 */
function getObjectFieldsSource(fields: Field[]): string {
  return fields
    .filter(([_, value]) => value !== undefined)
    .map(([key, value]) => `${key}: ${JSON.stringify(value)}`)
    .join(", ");
}

type Field = [string, string | undefined];
