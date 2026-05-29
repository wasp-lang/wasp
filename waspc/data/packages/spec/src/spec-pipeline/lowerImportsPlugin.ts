import assert from "node:assert/strict";
import path from "node:path";
import type { Plugin, RolldownMagicString } from "rolldown";
import type { ESTree as t } from "rolldown/utils";
import { WASP_SPEC_FILE_REGEX } from "./common.js";

interface PlannedImport {
  source: string;
  specifiers: { import: string; alias: string }[];
  start: number;
  end: number;
}

export function lowerImportsPlugin({
  projectRootDir,
}: {
  projectRootDir: string;
}): Plugin {
  const srcFolder = path.resolve(projectRootDir, "src");

  return {
    name: "wasp/spec/lower-imports",
    transform: {
      filter: { id: WASP_SPEC_FILE_REGEX },
      handler(code, id, { magicString }) {
        assert(magicString);

        const ast = this.parse(code, { lang: "ts" });

        const plannedImports = planLowerImports(ast).map((plannedImport) => ({
          ...plannedImport,
          source: path.join(
            "@src",
            path.relative(
              srcFolder,
              path.resolve(path.dirname(id), plannedImport.source),
            ),
          ),
        }));

        applyLowerImports(magicString, plannedImports);

        return { code: magicString };
      },
    },
  };

  function planLowerImports(ast: t.Program): PlannedImport[] {
    return ast.body
      .filter(
        (node): node is t.ImportDeclaration =>
          node.type === "ImportDeclaration" &&
          node.attributes.some(
            (attr) =>
              getString(attr.key) === "type" && getString(attr.value) === "ref",
          ),
      )
      .map((node) => {
        const source = getString(node.source);

        const specifiers = node.specifiers.map((specifier) => {
          switch (specifier.type) {
            case "ImportSpecifier": {
              return {
                import: getString(specifier.imported),
                alias: getString(specifier.local),
              };
            }
            case "ImportDefaultSpecifier": {
              return {
                import: "default",
                alias: getString(specifier.local),
              };
            }
            case "ImportNamespaceSpecifier": {
              throw new Error(
                `Unsupported import specifier type ${specifier.type} in ref import. Only named or default imports are supported.`,
              );
            }
            default:
              return specifier satisfies never;
          }
        });

        return {
          source,
          specifiers,
          start: node.start,
          end: node.end,
        };
      });
  }
}

function getString(node: t.IdentifierName | t.StringLiteral): string {
  if (node.type === "Identifier") {
    return node.name;
  } else {
    return node.value;
  }
}

function applyLowerImports(
  magicString: RolldownMagicString,
  plannedImports: PlannedImport[],
) {
  for (const plannedImport of plannedImports) {
    magicString.remove(plannedImport.start, plannedImport.end);

    for (const specifier of plannedImport.specifiers) {
      // Imports are hoisted, so we need to prepend the lowered imports regardless of where they were in the source file.
      magicString.prepend(
        `const ${specifier.alias} = ${JSON.stringify({ import: specifier.import, alias: specifier.alias, from: plannedImport.source })} as const;\n`,
      );
    }
  }
}
