import * as ts from "typescript";

export function createUniqueTopLevelNameGenerator(sourceFile: ts.SourceFile): {
  generateName(preferredName: string): string;
} {
  const usedNames = getTopLevelValueNames(sourceFile);

  return {
    generateName(preferredName) {
      const name = findUnusedName(preferredName, usedNames);
      usedNames.add(name);
      return name;
    },
  };
}

function getTopLevelValueNames(sourceFile: ts.SourceFile): Set<string> {
  return new Set(sourceFile.statements.flatMap(getTopLevelStatementValueNames));
}

function getTopLevelStatementValueNames(stmt: ts.Statement): string[] {
  if (ts.isImportDeclaration(stmt)) {
    return getImportValueNames(stmt);
  }

  if (ts.isImportEqualsDeclaration(stmt)) {
    return [stmt.name.text];
  }

  if (ts.isVariableStatement(stmt)) {
    return getVariableStatementValueNames(stmt);
  }

  if (
    ts.isFunctionDeclaration(stmt) ||
    ts.isClassDeclaration(stmt) ||
    ts.isEnumDeclaration(stmt)
  ) {
    return getDeclarationValueNames(stmt.name);
  }

  if (ts.isModuleDeclaration(stmt)) {
    return getModuleDeclarationValueNames(stmt);
  }

  return [];
}

function getImportValueNames(stmt: ts.ImportDeclaration): string[] {
  const importClause = stmt.importClause;
  if (!importClause || importClause.isTypeOnly) {
    return [];
  }

  const defaultImportNames = getDeclarationValueNames(importClause.name);

  const namedBindings = importClause.namedBindings;
  if (!namedBindings) {
    return defaultImportNames;
  }

  if (ts.isNamespaceImport(namedBindings)) {
    return [...defaultImportNames, namedBindings.name.text];
  }

  return [
    ...defaultImportNames,
    ...namedBindings.elements
      .filter((element) => !element.isTypeOnly)
      .map((element) => element.name.text),
  ];
}

function getVariableStatementValueNames(stmt: ts.VariableStatement): string[] {
  return stmt.declarationList.declarations.flatMap((declaration) =>
    getBindingValueNames(declaration.name),
  );
}

function getBindingValueNames(name: ts.BindingName): string[] {
  if (ts.isIdentifier(name)) {
    return [name.text];
  }

  return name.elements.flatMap((element) =>
    ts.isOmittedExpression(element) ? [] : getBindingValueNames(element.name),
  );
}

function getDeclarationValueNames(name: ts.Identifier | undefined): string[] {
  return name ? [name.text] : [];
}

function getModuleDeclarationValueNames(stmt: ts.ModuleDeclaration): string[] {
  return ts.isIdentifier(stmt.name) ? [stmt.name.text] : [];
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
