import { pathToFileURL } from "node:url";
import { parseAst } from "rolldown/parseAst";
import { planRefImportLowering } from "./refImportLowering.js";
import { planSourceAwareRefHelper } from "./sourceAwareRefHelper.js";

export type SourceRemoval = {
  start: number;
  end: number;
};

type MutableSource = {
  prepend(source: string): unknown;
  remove(start: number, end: number): unknown;
};

export function transformWaspTsSpecFile_mutate(
  source: MutableSource,
  {
    sourceText,
    sourcePath,
  }: {
    sourceText: string;
    sourcePath: string;
  },
): void {
  const program = parseAst(sourceText, { lang: "ts" });

  const sourceAwareRefHelperPlan = planSourceAwareRefHelper({
    program,
    sourceText,
    sourceFileUrl: toFileUrl(sourcePath),
  });
  const refImportLoweringPlan = planRefImportLowering({
    program,
    refHelperName: sourceAwareRefHelperPlan.refHelperName,
  });

  const prependedSource = [
    sourceAwareRefHelperPlan.source,
    refImportLoweringPlan.source,
  ]
    .filter((source) => source.length > 0)
    .join("");

  if (prependedSource.length > 0) {
    // Imports are hoisted, so we always prepend generated imports and lowered
    // ref bindings regardless of where the original ref import lived.
    source.prepend(prependedSource);
  }

  for (const removal of [
    ...sourceAwareRefHelperPlan.removals,
    ...refImportLoweringPlan.removals,
  ]) {
    source.remove(removal.start, removal.end);
  }
}

function toFileUrl(sourcePath: string): string {
  return sourcePath.startsWith("file:")
    ? sourcePath
    : pathToFileURL(sourcePath).href;
}
