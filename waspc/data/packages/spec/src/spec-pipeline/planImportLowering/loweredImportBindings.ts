import * as ts from "typescript";
import type { RefImportDescriptor } from "../../spec/refImport.js";

export type LoweredImportBinding = RefImportBinding | NamespaceImportBinding;

type RefImportBinding = {
  kind: "refImport";
  localName: string;
  descriptor: RefImportDescriptor;
};

export type NamespaceImportBinding = {
  kind: "namespace";
  localName: string;
  from: string;
  aliasPrefix: string;
};

export function getLoweredImportBindings(
  clause: ts.ImportClause,
  from: string,
): LoweredImportBinding[] {
  const bindings: LoweredImportBinding[] = [];

  if (clause.name) {
    const name = clause.name.text;
    bindings.push({
      kind: "refImport",
      localName: name,
      descriptor: { importDefault: name, from },
    });
  }

  const namedBindings = clause.namedBindings;
  if (namedBindings && ts.isNamespaceImport(namedBindings)) {
    const name = namedBindings.name.text;
    bindings.push({
      kind: "namespace",
      localName: name,
      from,
      aliasPrefix: getNamespaceAliasPrefix(name),
    });
  } else if (namedBindings) {
    for (const spec of namedBindings.elements) {
      const localName = spec.name.text;
      const importedName = getImportedName(spec);
      bindings.push({
        kind: "refImport",
        localName,
        descriptor: {
          import: importedName,
          from,
          ...(importedName === localName ? {} : { alias: localName }),
        },
      });
    }
  }

  return bindings;
}

/**
 * Users can avoid local collisions with namespace imports:
 * `import * as ops from "./src/operations" with { type: "ref" }` and
 * `import * as legacyOps from "./src/legacyOperations" with { type: "ref" }`.
 * We preserve that in generated aliases: `ops.archive` becomes `ops_archive`,
 * while `legacyOps.archive` becomes `legacyOps_archive`.
 */
function getNamespaceAliasPrefix(namespaceName: string): string {
  return `${namespaceName}_`;
}

/**
 * In `import { exported as local }`, TS stores `exported` in `propertyName`
 * and `local` in `name`. For `import { local }`, `propertyName` is undefined.
 */
function getImportedName(spec: ts.ImportSpecifier): string {
  return spec.propertyName?.text ?? spec.name.text;
}
