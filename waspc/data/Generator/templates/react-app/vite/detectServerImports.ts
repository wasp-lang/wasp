import { type Plugin } from "vite";

export function detectServerImports(): Plugin {
  return {
    name: 'wasp-detect-server-imports',
    transform(code, id) {
      const isInDotWaspFolder = id.includes("/.wasp/");

      if (isInDotWaspFolder) {
        // Skip checking files in .wasp folder.
        return;
      }

      const regex = /\s*import\s+(?:(?:[\w*\s{},]*)\s+from\s+)?(['"`])([^'"`]+)\1\s*/g;

      for (const match of code.matchAll(regex)) {
        const importPath = match[2];
        const isServerImport = importPath.startsWith("wasp/server");
        if (isServerImport) {
          const fileName = id.split('/').pop();
          throw new Error(
            `${fileName} contains a server import from ${importPath}. This is not allowed in the client code.`
          );
        }
      }
    },
  };
}
