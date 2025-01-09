import { type Plugin } from "vite";

export function detectServerImports(): Plugin {
  return {
    name: 'wasp-detect-server-imports',
    transform(code, filePath) {
      const isInDotWaspFolder = filePath.includes("/.wasp/");

      // We don't want to check for server imports in the Wasp 
      // framework code.
      if (isInDotWaspFolder) {
        return;
      }

      const imports = getImportsFromCode(code);

      for (const imp of imports) {
        if (imp.moduleName.startsWith("wasp/server")) {
          throw new Error(getServerImportErrorMessage(imp, filePath));
        }
      }
    },
  };
}

type Import = {
  importStatement: string;
  moduleName: string;
}

const importStatementRegex = /\s*import\s+(?:(?:[\w*\s{},]*)\s+from\s+)?(['"`])([^'"`]+)\1\s*/g;

function* getImportsFromCode(code: string): Generator<Import> {
  const matches = code.matchAll(importStatementRegex);
  for (const match of matches) {
    yield {
      importStatement: match[0].trim(),
      moduleName: match[2],
    };
  }
}

function getServerImportErrorMessage(imp: Import, filePath: string): string {
  return  `Client module ${getRelativeFilePath(filePath)} imports server specific module ${imp.moduleName}:

${imp.importStatement}

This is not allowed in the client code.`
}

// TODO: implement this with Wasp project dir passed to template.
function getRelativeFilePath(filePath: string): string {
  return filePath;
}
