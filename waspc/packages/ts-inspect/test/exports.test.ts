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
};

describe('exports.ts', () => {
  test('empty ts file has empty exports', async () => {
    expect(await getExportsOfFiles([testFiles.emptyFile])).toEqual({
      [testFiles.emptyFile]: []
    });
  });

  test('add file has just a default export', async () => {
    expect(await getExportsOfFiles([testFiles.addFile])).toEqual({
      [testFiles.addFile]: [
        { type: 'default', location: { line: 0, column: 0 } }
      ]
    });
  });

  test('complex file has default and normal export', async () => {
    expect(await getExportsOfFiles([testFiles.complexFile])).toEqual({
      [testFiles.complexFile]: [
        { type: 'default', location: { line: 0, column: 0 } },
        { type: 'named', name: 'isEven', location: { line: 4, column: 0 } },
        { type: 'named', name: 'isOdd', location: { line: 10, column: 0 } }
      ]
    });
  });

  test('dict_export file shows names for each export in dict', async () => {
    expect(await getExportsOfFiles([testFiles.dictExportFile])).toEqual({
      [testFiles.dictExportFile]: [
        { type: 'named', name: 'add' },
        { type: 'named', name: 'sub' },
      ],
    });
  })
});
