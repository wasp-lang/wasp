import type { ESTree as t } from "rolldown/utils";
import type { RefObjectDescriptor } from "../../spec/refObject.js";
import {
  getImportedName,
  getImportSourceValue,
  getLocalImportName,
  getStringValue,
  isImportDeclaration,
  isTypeOnlyImportDeclaration,
  isValueImportSpecifier,
  type NamedImportSpecifier,
} from "./importDeclarations.js";
import type { Removal } from "./transformWaspTsSpecFile.js";

export type RefImportLoweringPlan = {
  source: string;
  removals: Removal[];
};

type LoweredRefImportBinding = RefObjectBinding | NamespaceImportBinding;

type RefObjectBinding = {
  kind: "refObject";
  localName: string;
  descriptor: RefObjectDescriptor;
};

type NamespaceImportBinding = {
  kind: "namespace";
  localName: string;
  from: string;
  aliasPrefix: string;
};

export function planRefImportLowering({
  program,
  refName,
}: {
  program: t.Program;
  refName: string;
}): RefImportLoweringPlan {
  const replacements = program.body.flatMap((stmt) => {
    if (!isImportDeclaration(stmt)) {
      return [];
    }

    const refImportPath = getRefImportPath(stmt);
    if (!refImportPath) {
      return [];
    }

    return [
      {
        bindings: getLoweredImportBindings(stmt, refImportPath),
        remove: {
          start: stmt.start,
          end: stmt.end,
        },
      },
    ];
  });

  return {
    source: replacements
      .flatMap((replacement) => replacement.bindings)
      .map((binding) => getLoweredImportBindingSource(binding, refName))
      .join(""),
    removals: replacements.map((replacement) => replacement.remove),
  };
}

function getLoweredImportBindings(
  stmt: t.ImportDeclaration,
  from: string,
): LoweredRefImportBinding[] {
  if (isTypeOnlyImportDeclaration(stmt)) {
    return [];
  }

  const bindings: LoweredRefImportBinding[] = [];

  for (const specifier of stmt.specifiers) {
    const localName = getLocalImportName(specifier);

    switch (specifier.type) {
      case "ImportDefaultSpecifier":
        bindings.push({
          kind: "refObject",
          localName,
          descriptor: { importDefault: localName, from },
        });
        break;

      case "ImportNamespaceSpecifier":
        bindings.push({
          kind: "namespace",
          localName,
          from,
          aliasPrefix: `${localName}_`,
        });
        break;

      case "ImportSpecifier":
        if (isValueImportSpecifier(specifier)) {
          bindings.push(getNamedRefObjectBinding(specifier, from));
        }
        break;
    }
  }

  return bindings;
}

function getNamedRefObjectBinding(
  specifier: NamedImportSpecifier,
  from: string,
): RefObjectBinding {
  const localName = getLocalImportName(specifier);
  const importedName = getImportedName(specifier);

  return {
    kind: "refObject",
    localName,
    descriptor: {
      import: importedName,
      from,
      ...(importedName === localName ? {} : { alias: localName }),
    },
  };
}

function getLoweredImportBindingSource(
  binding: LoweredRefImportBinding,
  refName: string,
): string {
  switch (binding.kind) {
    case "refObject":
      return `const ${binding.localName} = ${refName}(${getRefObjectDescriptorObjectLiteralSource(binding.descriptor)});\n`;
    case "namespace":
      return getNamespaceImportProxySource(binding, refName);
  }
}

/**
 * Namespace imports are lowered to a Proxy so `import * as ops` can support any
 * `ops.anything` access by returning `ref({ import: "anything", ... })`. This
 * avoids enumerating every place where `ops` is used.
 *
 * The generated alias is prefixed with the local name (e.g. `ops.archive`
 * becomes `ops_archive`) so multiple namespace imports don't collide.
 */
function getNamespaceImportProxySource(
  binding: NamespaceImportBinding,
  refName: string,
): string {
  const from = JSON.stringify(binding.from);
  const aliasPrefix = JSON.stringify(binding.aliasPrefix);

  return `const ${binding.localName} = new Proxy({}, { get: (_t, k) => ${refName}({ import: String(k), from: ${from}, alias: ${aliasPrefix} + String(k) }) }) as Record<string, ReturnType<typeof ${refName}>>;\n`;
}

function getRefObjectDescriptorObjectLiteralSource(
  descriptor: RefObjectDescriptor,
): string {
  if ("import" in descriptor) {
    return getObjectLiteralSource([
      ["import", descriptor.import],
      ["from", descriptor.from],
      ["alias", descriptor.alias],
    ]);
  }

  return getObjectLiteralSource([
    ["importDefault", descriptor.importDefault],
    ["from", descriptor.from],
  ]);
}

function getObjectLiteralSource(
  fields: [string, string | undefined][],
): string {
  return `{ ${fields
    .filter((field): field is [string, string] => field[1] !== undefined)
    .map(([key, value]) => `${key}: ${JSON.stringify(value)}`)
    .join(", ")} }`;
}

function getRefImportPath(stmt: t.ImportDeclaration): string | undefined {
  if (!hasOnlyRefImportAttribute(stmt.attributes)) {
    return undefined;
  }

  return getImportSourceValue(stmt);
}

function hasOnlyRefImportAttribute(
  attributes: t.ImportDeclaration["attributes"],
): boolean {
  if (!attributes || attributes.length !== 1) {
    return false;
  }

  const [attribute] = attributes;
  return (
    attribute !== undefined &&
    getStringValue(attribute.key) === "type" &&
    getStringValue(attribute.value) === "ref"
  );
}
