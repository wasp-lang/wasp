import * as path from 'path';
import { getExportsOfFiles } from "../src/exports";

/**
 * Get an absolute path to a test file
 * @param filename Name of test file inside __dirname/exportTests directory
 */
function testFile(filename: string): string {
  return path.join(__dirname, 'exportTests', filename);
}

const testFiles = {
  emptyFile: testFile('empty.ts'),
  addFile: testFile('add.ts'),
  complexFile: testFile('complex.ts'),
  dictExportFile: testFile('dict_export.ts'),
  constExportFile: testFile('const_export.ts'),

  emptyTsconfig: testFile('tsconfig.json'),
};

describe('exports.ts', () => {
  test('empty ts file has empty exports', async () => {
    const request = { filenames: [testFiles.emptyFile] };
    expect(await getExportsOfFiles(request)).toEqual({
      [testFiles.emptyFile]: []
    });
  });

  test('add file has just a default export', async () => {
    const request = { filenames: [testFiles.addFile] };
    expect(await getExportsOfFiles(request)).toEqual({
      [testFiles.addFile]: [
        { type: 'default', location: { line: 0, column: 0 } }
      ]
    });
  });

  test('complex file has default and normal export', async () => {
    const request = { filenames: [testFiles.complexFile] };
    expect(await getExportsOfFiles(request)).toEqual({
      [testFiles.complexFile]: [
        { type: 'default', location: { line: 0, column: 0 } },
        { type: 'named', name: 'isEven', location: { line: 4, column: 0 } },
        { type: 'named', name: 'isOdd', location: { line: 10, column: 0 } }
      ]
    });
  });

  test('dict_export file shows names for each export in dict', async () => {
    const request = { filenames: [testFiles.dictExportFile] };
    expect(await getExportsOfFiles(request)).toEqual({
      [testFiles.dictExportFile]: [
        { type: 'named', name: 'add' },
        { type: 'named', name: 'sub' },
      ],
    });
  });

  test('empty ts file works with empty tsconfig', async () => {
    const request = { filenames: [testFiles.emptyFile], tsconfig: testFiles.emptyTsconfig };
    expect(await getExportsOfFiles(request)).toEqual({
      [testFiles.emptyFile]: []
    });
  });

  test('`export const` shows up in export list', async () => {
    const request = { filenames: [testFiles.constExportFile] };
    expect(await getExportsOfFiles(request)).toEqual({
      [testFiles.constExportFile]: [
        { type: 'named', name: 'isEven', location: { line: 0, column: 13 } }
      ]
    });
  })
});
