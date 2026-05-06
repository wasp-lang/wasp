import * as ts from "typescript";

const SRC_IMPORT_PREFIX = "./src/";
const SRC_DESCRIPTOR_PREFIX = "@src/";
const ACCEPTED_IMPORT_SHAPES =
  "Use default, named, aliased named, or namespace imports from ./src/*.";
const SUPPORTED_IMPORTS =
  "Use package imports for dependencies and ./src/* imports for external Wasp code references.";

/**
 * Rewrites top-level imports of the form `./src/*` into inline ExtImport
 * descriptor consts so the file no longer depends on user source modules at
 * runtime. The rewriter is purely textual: it parses a single SourceFile,
 * collects edit ranges for matching ImportDeclarations, and splices them.
 */
export function rewrite(sourceText: string): string {
  const sourceFile = ts.createSourceFile(
    "input.ts",
    sourceText,
    ts.ScriptTarget.ES2022,
    true,
    ts.ScriptKind.TS,
  );

  const edits: Edit[] = [];
  for (const stmt of sourceFile.statements) {
    if (ts.isExportDeclaration(stmt)) {
      validateExportDeclaration(sourceFile, stmt);
      continue;
    }

    if (!ts.isImportDeclaration(stmt)) continue;
    const specifier = getStringModuleSpecifier(stmt.moduleSpecifier);
    if (!specifier) continue;
    if (!isSrcImportSpecifier(specifier)) {
      validateNonSrcImport(sourceFile, stmt, specifier);
      continue;
    }

    validateImportDeclaration(sourceFile, stmt, specifier);

    const descriptorPath =
      SRC_DESCRIPTOR_PREFIX + specifier.slice(SRC_IMPORT_PREFIX.length);
    edits.push({
      start: stmt.getStart(sourceFile),
      end: stmt.getEnd(),
      replacement: emitConsts(stmt.importClause!, descriptorPath),
    });
  }

  return applyEdits(sourceText, edits);
}

type Edit = { start: number; end: number; replacement: string };

function getStringModuleSpecifier(
  moduleSpecifier: ts.Expression,
): string | undefined {
  return ts.isStringLiteral(moduleSpecifier) ? moduleSpecifier.text : undefined;
}

function isSrcImportSpecifier(specifier: string): boolean {
  return specifier.startsWith(SRC_IMPORT_PREFIX);
}

function isRelativeImportSpecifier(specifier: string): boolean {
  return specifier.startsWith("./") || specifier.startsWith("../");
}

function validateNonSrcImport(
  sourceFile: ts.SourceFile,
  stmt: ts.ImportDeclaration,
  specifier: string,
): void {
  if (!isRelativeImportSpecifier(specifier)) return;

  throwUnsupportedImport(
    sourceFile,
    stmt,
    specifier,
    `Relative imports outside ./src/* are not supported. ${SUPPORTED_IMPORTS}`,
  );
}

function validateImportDeclaration(
  sourceFile: ts.SourceFile,
  stmt: ts.ImportDeclaration,
  specifier: string,
): void {
  const clause = stmt.importClause;
  if (!clause) {
    throwUnsupportedImport(
      sourceFile,
      stmt,
      specifier,
      "Side-effect imports are not supported.",
    );
  }

  if (clause.isTypeOnly) {
    throwUnsupportedImport(
      sourceFile,
      stmt,
      specifier,
      "Type-only imports are not supported.",
    );
  }

  const bindings = clause.namedBindings;
  if (bindings && ts.isNamedImports(bindings)) {
    if (!clause.name && bindings.elements.length === 0) {
      throwUnsupportedImport(
        sourceFile,
        stmt,
        specifier,
        "Empty named imports are not supported.",
      );
    }

    if (bindings.elements.some((element) => element.isTypeOnly)) {
      throwUnsupportedImport(
        sourceFile,
        stmt,
        specifier,
        "Mixed type/value imports are not supported.",
      );
    }
  }
}

function validateExportDeclaration(
  sourceFile: ts.SourceFile,
  stmt: ts.ExportDeclaration,
): void {
  if (!stmt.moduleSpecifier || !ts.isStringLiteral(stmt.moduleSpecifier))
    return;
  const specifier = stmt.moduleSpecifier.text;
  if (!isSrcImportSpecifier(specifier)) {
    if (isRelativeImportSpecifier(specifier)) {
      throwUnsupportedImport(
        sourceFile,
        stmt,
        specifier,
        `Relative re-exports are not supported. ${SUPPORTED_IMPORTS}`,
      );
    }
    return;
  }

  throwUnsupportedImport(
    sourceFile,
    stmt,
    specifier,
    "Re-exports are not supported.",
  );
}

function throwUnsupportedImport(
  sourceFile: ts.SourceFile,
  node: ts.Node,
  specifier: string,
  reason: string,
): never {
  const { line, character } = sourceFile.getLineAndCharacterOfPosition(
    node.getStart(sourceFile),
  );
  throw new Error(
    `Unsupported ./src import in main.wasp.ts at ${line + 1}:${character + 1}: ${reason} Found ${JSON.stringify(specifier)}. ${ACCEPTED_IMPORT_SHAPES}`,
  );
}

function applyEdits(text: string, edits: Edit[]): string {
  edits.sort((a, b) => a.start - b.start);
  let out = "";
  let cursor = 0;
  for (const e of edits) {
    out += text.slice(cursor, e.start) + e.replacement;
    cursor = e.end;
  }
  return out + text.slice(cursor);
}

function emitConsts(clause: ts.ImportClause, descriptorPath: string): string {
  const lines: string[] = [];

  if (clause.name) {
    const name = clause.name.text;
    lines.push(
      `const ${name} = { importDefault: ${JSON.stringify(name)}, from: ${JSON.stringify(descriptorPath)} } as const;`,
    );
  }

  const bindings = clause.namedBindings;
  if (bindings && ts.isNamespaceImport(bindings)) {
    const name = bindings.name.text;
    lines.push(
      `const ${name} = new Proxy({}, { get: (_t, k) => ({ import: String(k), from: ${JSON.stringify(descriptorPath)}, alias: ${JSON.stringify(`${name}_`)} + String(k) } as const) }) as Record<string, { import: string; from: ${JSON.stringify(descriptorPath)}; alias: string }>;`,
    );
  } else if (bindings) {
    for (const spec of bindings.elements) {
      const local = spec.name.text;
      const imported = spec.propertyName?.text ?? local;
      const alias =
        imported === local ? "" : `, alias: ${JSON.stringify(local)}`;
      lines.push(
        `const ${local} = { import: ${JSON.stringify(imported)}, from: ${JSON.stringify(descriptorPath)}${alias} } as const;`,
      );
    }
  }

  return lines.join("\n");
}
