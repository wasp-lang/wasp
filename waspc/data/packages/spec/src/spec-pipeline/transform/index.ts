import { ensureSourceAwareRefImport } from "./ensureSourceAwareRefImport.js";
import { lowerRefImports } from "./lowerRefImports.js";
import { planImportLowering } from "./planImportLowering/index.js";

/**
 * Transforms the Wasp Spec file source:
 * 1. Makes sure `refImport` helper is constructed from `makeRefImport`.
 * 2. Lowers the `with { type: "ref" }` imports.
 */
export function transformWaspTsSpecSource({
  sourceText,
  sourcePath,
}: {
  sourceText: string;
  sourcePath: string;
}): string {
  // TODO: We do this to cause diagnostics in the original source before adding
  // the ref import helper.
  // Check if `magic-string` can help us compose multiple edits while
  // keeping diagnostics mapped to the original source, so we don't plan twice.
  planImportLowering({ sourceText, sourcePath });

  const { sourceText: sourceAwareRefImportSource, refImportName } =
    ensureSourceAwareRefImport({
      sourceText,
      sourcePath,
    });

  return lowerRefImports({
    sourceText: sourceAwareRefImportSource,
    sourcePath,
    refImportName,
  });
}
