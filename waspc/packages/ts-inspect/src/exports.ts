import ts from 'typescript';
import * as fs from 'fs/promises';
import * as path from 'path';
import * as JSON5 from 'json5';

export type Export
  = { type: 'default' }
  | { type: 'named', name: string }

export async function getExportsOfFiles(
  filenames: string[],
  tsconfigFilename?: string,
): Promise<{ [file: string]: Export[] }> {
  let compilerOptions: ts.CompilerOptions = {};

  if (tsconfigFilename) {
    const configJson = JSON5.parse(await fs.readFile(tsconfigFilename, 'utf8'));
    const basePath = path.dirname(tsconfigFilename)

    const { options, errors } = ts.convertCompilerOptionsFromJson(
      configJson.compilerOptions, basePath, tsconfigFilename
    );
    if (errors && errors.length) {
      throw errors;
    }
    compilerOptions = options;
  }

  const exportsMap: { [file: string]: Export[] } = {};

  const program = ts.createProgram(filenames, compilerOptions);
  const checker = program.getTypeChecker();
  for (let filename of filenames) {
    const source = program.getSourceFile(filename);
    if (!source) {
      console.error(`Error getting source for ${filename}`)
      continue;
    }
    const moduleSymbol = checker.getSymbolAtLocation(source);
    if (!moduleSymbol) {
      console.error(`Error getting moduleSymbol for ${filename}`);
      continue;
    }
    const exports = checker.getExportsOfModule(moduleSymbol);

    exportsMap[filename] = exports.map(exp => {
      const exportName = exp.getName();
      if (exportName === 'default') {
        return { type: 'default' };
      } else {
        return { type: 'named', name: exportName };
      }
    });
  }

  return exportsMap;
}
