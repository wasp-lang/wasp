import type { ESTree as t } from "rolldown/utils";
import {
  getLocalImportName,
  isImportDeclaration,
  isTypeOnlyImportDeclaration,
  isValueImportSpecifier,
  type NamedImportSpecifier,
} from "./importDeclarations.js";

export function createUniqueTopLevelNameGenerator(program: t.Program): {
  generateName(preferredName: string): string;
} {
  const usedNames = getTopLevelValueNames(program);

  return {
    generateName(preferredName) {
      const name = findUnusedName(preferredName, usedNames);
      usedNames.add(name);
      return name;
    },
  };
}

function findUnusedName(
  preferredName: string,
  usedNames: ReadonlySet<string>,
): string {
  if (!usedNames.has(preferredName)) {
    return preferredName;
  }

  for (let index = 1; ; index++) {
    const candidate = `${preferredName}${index}`;
    if (!usedNames.has(candidate)) {
      return candidate;
    }
  }
}

function getTopLevelValueNames(program: t.Program): Set<string> {
  return new Set(program.body.flatMap(getTopLevelStatementValueNames));
}

function getTopLevelStatementValueNames(stmt: t.Statement): string[] {
  if (isImportDeclaration(stmt)) {
    return getImportValueNames(stmt);
  }

  if (stmt.type === "ExportNamedDeclaration") {
    return stmt.declaration
      ? getTopLevelDeclarationValueNames(stmt.declaration)
      : [];
  }

  if (stmt.type === "ExportDefaultDeclaration") {
    return getTopLevelDeclarationValueNames(stmt.declaration);
  }

  return getTopLevelDeclarationValueNames(stmt);
}

function getTopLevelDeclarationValueNames(declaration: unknown): string[] {
  if (!isObject(declaration)) {
    return [];
  }

  switch (declaration.type) {
    case "TSImportEqualsDeclaration":
      return declaration.importKind === "type"
        ? []
        : getOptionalIdentifierName(declaration.id);

    case "VariableDeclaration":
      return getVariableDeclarationValueNames(declaration);

    case "FunctionDeclaration":
    case "ClassDeclaration":
    case "TSEnumDeclaration":
    case "TSModuleDeclaration":
      return getOptionalIdentifierName(declaration.id);

    default:
      return [];
  }
}

function getImportValueNames(stmt: t.ImportDeclaration): string[] {
  if (isTypeOnlyImportDeclaration(stmt)) {
    return [];
  }

  return stmt.specifiers.flatMap((specifier) => {
    if (
      specifier.type === "ImportSpecifier" &&
      !isValueImportSpecifier(specifier as NamedImportSpecifier)
    ) {
      return [];
    }

    return [getLocalImportName(specifier)];
  });
}

function getVariableDeclarationValueNames(declaration: {
  declarations?: unknown;
}): string[] {
  return Array.isArray(declaration.declarations)
    ? declaration.declarations.flatMap((declarator) =>
        getBindingValueNames(getObjectProperty(declarator, "id")),
      )
    : [];
}

function getBindingValueNames(binding: unknown): string[] {
  if (!isObject(binding)) {
    return [];
  }

  switch (binding.type) {
    case "Identifier":
      return typeof binding.name === "string" ? [binding.name] : [];

    case "ArrayPattern":
      return Array.isArray(binding.elements)
        ? binding.elements.flatMap(getBindingValueNames)
        : [];

    case "ObjectPattern":
      return Array.isArray(binding.properties)
        ? binding.properties.flatMap(getObjectPatternPropertyValueNames)
        : [];

    case "RestElement":
      return getBindingValueNames(binding.argument);

    case "AssignmentPattern":
      return getBindingValueNames(binding.left);

    default:
      return [];
  }
}

function getObjectPatternPropertyValueNames(property: unknown): string[] {
  if (!isObject(property)) {
    return [];
  }

  switch (property.type) {
    case "Property":
      return getBindingValueNames(property.value);
    case "RestElement":
      return getBindingValueNames(property.argument);
    default:
      return [];
  }
}

function getOptionalIdentifierName(node: unknown): string[] {
  return isObject(node) &&
    node.type === "Identifier" &&
    typeof node.name === "string"
    ? [node.name]
    : [];
}

function getObjectProperty(object: unknown, property: string): unknown {
  return isObject(object) ? object[property] : undefined;
}

function isObject(value: unknown): value is Record<string, unknown> {
  return typeof value === "object" && value !== null;
}
