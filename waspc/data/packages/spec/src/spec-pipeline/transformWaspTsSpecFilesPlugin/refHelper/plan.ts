import type { ESTree as t } from "rolldown/utils";
import { walk } from "estree-walker";
import {
  getStringValue,
  getTopLevelBindings,
  INTERNAL_MAKE_REF_HELPER_IMPORT_NAME,
  makeSafeName,
  PUBLIC_REF_HELPER_IMPORT_NAME,
  PUBLIC_REF_HELPER_IMPORT_SOURCE,
} from "../util.js";

export type Plan = {
  /**
   * The local names `ref` was imported under (e.g. `["ref"]` or `["appRef"]`),
   * one per `ref` specifier we removed. Each gets aliased back to the generated
   * helper so the rest of the file keeps working.
   */
  refHelperLocalNames: string[];
  /** Source ranges to delete */
  removals: SourceRange[];
  safeMakeRefHelperName: string;
  /** Source ranges of waspImport calls to rewrite */
  waspImportCalls: SourceRange[];
};

type SourceRange = { start: number; end: number };

export function planTransformRefHelper(ast: t.Program): Plan | null {
  const refHelperImportDeclarations = ast.body.filter(
    isRefHelperImportDeclaration,
  );

  if (refHelperImportDeclarations.length === 0) {
    return null;
  }

  const removals = mergeRanges(
    refHelperImportDeclarations.flatMap(planSpecifierRemovals),
  );

  const refHelperLocalNames = refHelperImportDeclarations.flatMap(
    (declaration) =>
      declaration.specifiers
        .filter(isRefHelperImportSpecifier)
        .map((specifier) => getStringValue(specifier.local)),
  );

  const waspImportLocalNames = refHelperImportDeclarations.flatMap(
    (declaration) =>
      declaration.specifiers
        .filter(isWaspImportSpecifier)
        .map((specifier) => getStringValue(specifier.local)),
  );

  const scope = getTopLevelBindings(ast);

  const safeMakeRefHelperName = makeSafeName(
    INTERNAL_MAKE_REF_HELPER_IMPORT_NAME,
    scope,
  );

  const waspImportCalls = findWaspImportCalls(
    ast,
    new Set(waspImportLocalNames),
  );

  return {
    refHelperLocalNames,
    removals,
    safeMakeRefHelperName,
    waspImportCalls,
  };
}

function isRefHelperImportDeclaration(
  node: t.Statement,
): node is t.ImportDeclaration {
  return (
    node.type === "ImportDeclaration" &&
    node.importKind === "value" &&
    getStringValue(node.source) === PUBLIC_REF_HELPER_IMPORT_SOURCE
  );
}

// We want to remove all `ref` and `waspImport` specifiers, with two caveats:
// - If they are the only specifiers in their import declaration, we remove the
//   whole declaration.
// - If there are multiple specifiers, we need to extend the range a bit to also
//   remove the comma next to it, as a leftover comma would cause a SyntaxError.
function planSpecifierRemovals(
  importDeclaration: t.ImportDeclaration,
): SourceRange[] {
  const specifiersCount = importDeclaration.specifiers.length;

  const specifiersToRemove = importDeclaration.specifiers
    .map((node, position) => ({ node, position }))
    .filter(
      ({ node }) => isRefHelperImportSpecifier(node) || isWaspImportSpecifier(node),
    );

  if (specifiersToRemove.length === 0) return [];
  if (specifiersToRemove.length === specifiersCount) return [importDeclaration];

  return specifiersToRemove.map(({ node: currentSpecifier, position }) => {
    const isFirstSpecifier = position === 0;

    if (isFirstSpecifier) {
      const nextSpecifier = importDeclaration.specifiers[position + 1];
      return {
        start: currentSpecifier.start,
        end: nextSpecifier?.start ?? currentSpecifier.end,
      };
    } else {
      const previousSpecifier = importDeclaration.specifiers[position - 1];
      return {
        start: previousSpecifier?.end ?? currentSpecifier.start,
        end: currentSpecifier.end,
      };
    }
  });
}

function isRefHelperImportSpecifier(
  node: t.ImportDeclarationSpecifier,
): node is t.ImportSpecifier {
  return (
    node.type === "ImportSpecifier" &&
    node.importKind === "value" &&
    getStringValue(node.imported) === PUBLIC_REF_HELPER_IMPORT_NAME
  );
}

// Check if it is the waspImport specifier
function isWaspImportSpecifier(
  node: t.ImportDeclarationSpecifier,
): node is t.ImportSpecifier {
  return (
    node.type === "ImportSpecifier" &&
    node.importKind === "value" &&
    getStringValue(node.imported) === "waspImport"
  );
}

function mergeRanges(ranges: SourceRange[]): SourceRange[] {
  if (ranges.length === 0) return [];
  const sorted = [...ranges].sort((a, b) => a.start - b.start);
  const first = sorted[0];
  if (!first) return [];
  const merged: SourceRange[] = [first];
  for (let i = 1; i < sorted.length; i++) {
    const last = merged[merged.length - 1];
    const current = sorted[i];
    if (!last || !current) continue;
    if (current.start <= last.end) {
      last.end = Math.max(last.end, current.end);
    } else {
      merged.push(current);
    }
  }
  return merged;
}

function findWaspImportCalls(
  ast: t.Program,
  localNames: ReadonlySet<string>,
): SourceRange[] {
  if (localNames.size === 0) return [];
  const calls: SourceRange[] = [];
  walk(
    // @ts-expect-error - AST types mismatch
    ast,
    {
      enter(node) {
        if (
          node.type === "CallExpression" &&
          node.callee.type === "Identifier" &&
          localNames.has(node.callee.name)
        ) {
          const callee = node.callee as any;
          if (typeof callee.start === "number" && typeof callee.end === "number") {
            calls.push({
              start: callee.start,
              end: callee.end,
            });
          }
        }
      },
    },
  );
  return calls;
}
