import ts from 'typescript';
import * as fs from 'fs/promises';
import * as path from 'path';
import * as JSON5 from 'json5';

export type Export
  = { type: 'default' } & Location
  | { type: 'named', name: string } & Location

export type Location = { location?: { line: number, column: number } }

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
      // This is caused by errors within the TS file, so we should just say that
      // there are no exports.
      exportsMap[filename] = [];
      continue;
    }
    const exports = checker.getExportsOfModule(moduleSymbol);

    exportsMap[filename] = exports.map(exp => {
      let location = undefined;
      if (exp.valueDeclaration) {
        // NOTE: This isn't a very precise location for the export: it just
        // goes to the `export` keyword. But getting more accurate seems really
        // difficult so let's just stick with this for now.
        const position = exp.valueDeclaration.getStart();
        const { line, character } = ts.getLineAndCharacterOfPosition(
          exp.valueDeclaration.getSourceFile(), position
        );
        location = { line, column: character };
      }

      const exportName = exp.getName();
      if (exportName === 'default') {
        return { type: 'default', location };
      } else {
        return { type: 'named', name: exportName, location };
      }
    });
  }

  return exportsMap;
}
