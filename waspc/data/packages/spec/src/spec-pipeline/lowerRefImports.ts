import type { RefImportDescriptor } from "../spec/refImport.js";
import { ensureSourceAwareRefImport } from "./ensureSourceAwareRefImport.js";
import type { ImportLoweringPlan } from "./planImportLowering/index.js";
import { planImportLowering } from "./planImportLowering/index.js";
import type {
  LoweredImportBinding,
  NamespaceImportBinding,
} from "./planImportLowering/loweredImportBindings.js";
import { applyEdits, type Edit } from "./sourceEdits.js";

/**
 * Given source code, finds supported ref import statements and replaces them
 * with inline refImport(...) consts. We call this lowering imports.
 */
export function lowerRefImports({
  sourceText,
  sourcePath,
}: {
  sourceText: string;
  sourcePath: string;
  projectRootDir: string;
}): string {
  const initialPlan = planImportLowering({ sourceText, sourcePath });
  const sourceAwareRefImport = ensureSourceAwareRefImport({
    sourceText,
    sourcePath,
    required: initialPlan.replacements.length > 0,
    helperDeclarationInsertionOffset: initialPlan.replacements[0]?.start,
  });
  const plan = planImportLowering({
    sourceText: sourceAwareRefImport.sourceText,
    sourcePath,
  });

  return applyEdits(
    sourceAwareRefImport.sourceText,
    getImportLoweringEdits(plan, sourceAwareRefImport.refImportName),
  );
}

function getImportLoweringEdits(
  plan: ImportLoweringPlan,
  refImportName: string,
): Edit[] {
  return plan.replacements.map((replacement) => ({
    start: replacement.start,
    end: replacement.end,
    text: getImportReplacementSource(replacement.bindings, refImportName),
  }));
}

function getImportReplacementSource(
  bindings: LoweredImportBinding[],
  refImportName: string,
): string {
  return bindings
    .map((binding) => getLoweredImportBindingSource(binding, refImportName))
    .join("\n");
}

function getLoweredImportBindingSource(
  binding: LoweredImportBinding,
  refImportName: string,
): string {
  switch (binding.kind) {
    case "refImport":
      return `const ${binding.localName} = ${refImportName}(${getRefImportDescriptorObjectLiteralSource(binding.descriptor)});`;
    case "namespace":
      return getNamespaceImportProxySource(binding, refImportName);
  }
}

/**
 * Namespace imports are lowered to a Proxy so `import * as ops` can support any
 * `ops.anything` access by returning `{ import: "anything", from: ... }`. This
 * avoids enumerating every place where `ops` is used.
 */
function getNamespaceImportProxySource(
  binding: NamespaceImportBinding,
  refImportName: string,
): string {
  const from = JSON.stringify(binding.from);
  const aliasPrefix = JSON.stringify(binding.aliasPrefix);

  return `const ${binding.localName} = new Proxy({}, { get: (_t, k) => ${refImportName}({ import: String(k), from: ${from}, alias: ${aliasPrefix} + String(k) }) }) as Record<string, ReturnType<typeof ${refImportName}>>;`;
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
