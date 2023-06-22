import ts from 'typescript';
import * as fs from 'fs/promises';
import * as path from 'path';
import JSON5 from 'json5';
import { z } from 'zod';

export const ExportRequest = z.object({
  tsconfig: z.string().optional(),
  filenames: z.array(z.string())
});

export const ExportRequests = z.array(ExportRequest);

export type ExportRequest = z.infer<typeof ExportRequest>;

export type Export
  = { type: 'default' } & Range
  | { type: 'named', name: string } & Range

export type Range = { range?: { start: Location, end: Location } }

export type Location = { line: number, column: number }

export async function getExportsOfFiles(request: ExportRequest): Promise<{ [file: string]: Export[] }> {
  let compilerOptions: ts.CompilerOptions = {};

  // If a tsconfig is given, load the configuration.
  if (request.tsconfig) {
    const configJson = JSON5.parse(await fs.readFile(request.tsconfig, 'utf8'));
    const basePath = path.dirname(request.tsconfig)

    const { options, errors } = ts.convertCompilerOptionsFromJson(
      configJson.compilerOptions, basePath, request.tsconfig
    );
    if (errors && errors.length) {
      throw errors;
    }
    compilerOptions = options;
  }

  const exportsMap: { [file: string]: Export[] } = {};

  // Initialize the TS compiler.
  const program = ts.createProgram(request.filenames, compilerOptions);
  const checker = program.getTypeChecker();

  // Loop through each given file and try to get its exports.
  for (let filename of request.filenames) {
    try {
      exportsMap[filename] = getExportsForFile(program, checker, filename);
    } catch (err) {
      console.error(err);
      exportsMap[filename] = [];
    }
  }

  return exportsMap;
}

function getExportsForFile(program: ts.Program, checker: ts.TypeChecker, filename: string): Export[] {
  const source = program.getSourceFile(filename);
  if (!source) {
    throw new Error(`Error getting source for ${filename}`);
  }
  const moduleSymbol = checker.getSymbolAtLocation(source);
  if (!moduleSymbol) {
    // This is caused by errors within the TS file, so we say there are no exports.
    return [];
  }
  const exports = checker.getExportsOfModule(moduleSymbol);
  return exports.map(exp => getExportForExportSymbol(program, checker, exp));
}

function getExportForExportSymbol(program: ts.Program, checker: ts.TypeChecker, exp: ts.Symbol): Export {
  let range = undefined;
  if (exp.valueDeclaration) {
    // NOTE: This isn't a very robust way of getting the location: it will always
    // point to the line that has `export`, rather than the line where the exported
    // symbol is defined.
    const startOffset = exp.valueDeclaration.getStart();
    const startPos = ts.getLineAndCharacterOfPosition(
      exp.valueDeclaration.getSourceFile(), startOffset
    );
    const endOffset = exp.valueDeclaration.getEnd();
    const endPos = ts.getLineAndCharacterOfPosition(
      exp.valueDeclaration.getSourceFile(), endOffset
    )
    range = {
      start: { line: startPos.line, column: startPos.character },
      end: { line: endPos.line, column: endPos.character }
    };
  }

  // Convert export to the output format.
  const exportName = exp.getName();
  if (exportName === 'default') {
    return { type: 'default', range };
  } else {
    return { type: 'named', name: exportName, range };
  }
}
