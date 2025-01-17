import { type Plugin } from "vite";
import path from "path";

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
  return  `Client module "${getRelativeFilePath(filePath)}" imports server code:

${imp.importStatement}

This is not supported in the client code.`;
}

const waspProjectDirAbsPath = getWaspProjectDirAbsPathFromCwd();

function getRelativeFilePath(filePath: string): string {
  return filePath.replace(waspProjectDirAbsPath, "");
}

// We are not passing the waspProjectDir path from Haskell because
// our e2e tests stop working. Becuase we need to absolute path of the
// Wasp project directory, it contains things like the username of the
// user running the tests, which is different on different machines.
function getWaspProjectDirAbsPathFromCwd(): string {
  const webAppDirAbsPath = process.cwd();
  const waspProjectDirAbsPath = path.join(webAppDirAbsPath, "../../../");
  return waspProjectDirAbsPath;
}
