import ts from 'typescript';
import * as fs from 'fs/promises';
import * as path from 'path';
import JSON5 from 'json5';
import { z } from 'zod';

export const ExportsRequest = z.object({
  tsconfig: z.string().optional(),
  filepaths: z.array(z.string())
});

export const ExportsRequests = z.array(ExportsRequest);

export type ExportsRequest = z.infer<typeof ExportsRequest>;

export type Export
  = { type: 'default' } & Range
  | { type: 'named', name: string } & Range

export type Range = { range?: { start: Location, end: Location } }

export type Location = { line: number, column: number }

export async function getExportsOfFiles(request: ExportsRequest): Promise<{ [file: string]: Export[] }> {
  let compilerOptions: ts.CompilerOptions = {};

  // If a tsconfig is given, load the configuration.
  if (request.tsconfig) {
    compilerOptions = await loadCompilerOptionsFromTsConfig(request.tsconfig);
  }

  const exportsMap: { [file: string]: Export[] } = {};

  // Initialize the TS compiler.
  const program = ts.createProgram(request.filepaths, compilerOptions);
  const checker = program.getTypeChecker();

  // Loop through each given file and try to get its exports.
  for (let filename of request.filepaths) {
    try {
      exportsMap[filename] = getExportsOfFile(program, checker, filename);
    } catch (err) {
      console.error(err);
      exportsMap[filename] = [];
    }
  }

  return exportsMap;
}

async function loadCompilerOptionsFromTsConfig(tsconfig: string): Promise<ts.CompilerOptions> {
  const configJson = JSON5.parse(await fs.readFile(tsconfig, 'utf8'));
  const basePath = path.dirname(tsconfig)

  const { options, errors } = ts.convertCompilerOptionsFromJson(
    configJson.compilerOptions, basePath, tsconfig
  );
  if (errors && errors.length) {
    throw errors;
  }
  return options;
}

function getExportsOfFile(program: ts.Program, checker: ts.TypeChecker, filename: string): Export[] {
  const source = program.getSourceFile(filename);
  if (!source) {
    throw new Error(`Error getting source for ${filename}`);
  }
  const moduleSymbol = checker.getSymbolAtLocation(source);
  if (!moduleSymbol) {
    // This is caused by errors within the TS file, so we say there are no exports.
    return [];
  }
  const exportSymbols = checker.getExportsOfModule(moduleSymbol);
  return exportSymbols.map(exp => getExportForExportSymbol(program, checker, exp));
}

function getExportForExportSymbol(program: ts.Program, checker: ts.TypeChecker, exportSymbol: ts.Symbol): Export {
  let range = undefined;
  // Try to get the location information from the first value declaration for
  // the export. If there are no value declarations, we don't return any location
  // info for this export and `range` remains undefined.
  if (exportSymbol.valueDeclaration) {
    // NOTE: This isn't a very robust way of getting the location: it will always
    // point to the line that has `export`, rather than the line where the exported
    // symbol is defined.
    const startOffset = exportSymbol.valueDeclaration.getStart();
    const startPos = ts.getLineAndCharacterOfPosition(
      exportSymbol.valueDeclaration.getSourceFile(), startOffset
    );
    const endOffset = exportSymbol.valueDeclaration.getEnd();
    const endPos = ts.getLineAndCharacterOfPosition(
      exportSymbol.valueDeclaration.getSourceFile(), endOffset
    )
    range = {
      start: { line: startPos.line, column: startPos.character },
      end: { line: endPos.line, column: endPos.character }
    };
  }

  // Convert export to the output format.
  const exportName = exportSymbol.getName();
  if (exportName === 'default') {
    return { type: 'default', range };
  } else {
    return { type: 'named', name: exportName, range };
  }
}
