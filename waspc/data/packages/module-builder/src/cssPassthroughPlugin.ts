import { copyFileSync, existsSync, mkdirSync } from "node:fs";
import path from "node:path";
import type { Plugin } from "rolldown";

// Module components style themselves with side-effect CSS imports
// (`import "./X.css"`). The import specifier stays verbatim in the compiled
// JavaScript and the CSS file is copied into the output directory mirroring
// its source path, so the host bundler resolves it like any other package CSS.
export function cssPassthroughPlugin({
  sourceDir,
  outDir,
}: {
  sourceDir: string;
  outDir: string;
}): Plugin {
  const cssFilesToCopy = new Map<string, string>();

  return {
    name: "wasp/module-builder/css-passthrough",

    resolveId(source, importer) {
      if (!source.endsWith(".css") || importer === undefined) {
        return null;
      }
      if (!source.startsWith("./") && !source.startsWith("../")) {
        return null;
      }

      const cssFilePath = path.resolve(path.dirname(importer), source);
      const cssFileRelativePath = tryGetSourceRelativePath(
        sourceDir,
        cssFilePath,
      );
      if (cssFileRelativePath === undefined) {
        throw new Error(
          `${importer} imports ${JSON.stringify(source)}, which resolves to ` +
            `${cssFilePath}, outside ${sourceDir}. Module CSS files must live inside src/.`,
        );
      }
      if (!existsSync(cssFilePath)) {
        throw new Error(
          `${importer} imports ${JSON.stringify(source)}, but ${cssFilePath} does not exist.`,
        );
      }

      cssFilesToCopy.set(cssFilePath, path.join(outDir, cssFileRelativePath));

      // Marking the import external keeps the specifier verbatim in the output.
      return { id: source, external: true };
    },

    writeBundle() {
      for (const [cssFilePath, outFilePath] of cssFilesToCopy) {
        mkdirSync(path.dirname(outFilePath), { recursive: true });
        copyFileSync(cssFilePath, outFilePath);
      }
    },
  };
}

function tryGetSourceRelativePath(
  sourceDir: string,
  filePath: string,
): string | undefined {
  const relativePath = path.relative(sourceDir, filePath);
  if (
    path.isAbsolute(relativePath) ||
    relativePath === ".." ||
    relativePath.startsWith(`..${path.sep}`)
  ) {
    return undefined;
  }

  return relativePath;
}
