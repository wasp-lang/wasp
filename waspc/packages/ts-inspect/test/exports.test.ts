import * as path from 'path';
import { getExportsOfFiles } from "../src/exports";

// TODO(before merge): run these tests in CI

/**
 * Get an absolute path to a test file
 * @param filename Name of test file inside __dirname/exportTests directory
 */
function getTestFilePath(filename: string): string {
  return path.join(__dirname, 'exportTests', filename);
}

const testFiles = {
  emptyFile: getTestFilePath('empty.ts'),
  addFile: getTestFilePath('add.ts'),
  complexFile: getTestFilePath('complex.ts'),
  dictExportFile: getTestFilePath('dict_export.ts'),
  constExportFile: getTestFilePath('const_export.ts'),

  emptyTsconfig: getTestFilePath('tsconfig.json'),
};

describe('exports.ts', () => {
  test('empty ts file has empty exports', async () => {
    const request = { filepaths: [testFiles.emptyFile] };
    expect(await getExportsOfFiles(request)).toEqual({
      [testFiles.emptyFile]: []
    });
  });

  test('add file has just a default export', async () => {
    const request = { filepaths: [testFiles.addFile] };
    expect(await getExportsOfFiles(request)).toEqual({
      [testFiles.addFile]: [{
        type: 'default',
        range: {
          start: { line: 0, column: 0 },
          end: { line: 2, column: 1 }
        }
      }]
    });
  });

  test('complex file has default and normal export', async () => {
    const request = { filepaths: [testFiles.complexFile] };
    expect(await getExportsOfFiles(request)).toEqual({
      [testFiles.complexFile]: [
        {
          type: 'default',
          range: {
            start: { line: 0, column: 0 },
            end: { line: 2, column: 1 }
          }
        },
        {
          type: 'named', name: 'isEven',
          range: {
            start: { line: 4, column: 0 },
            end: { line: 8, column: 1 }
          }
        },
        {
          type: 'named', name: 'isOdd',
          range: {
            start: { line: 10, column: 0 },
            end: { line: 14, column: 1 }
          }
        }
      ]
    });
  });

  test('dict_export file shows names for each export in dict', async () => {
    const request = { filepaths: [testFiles.dictExportFile] };
    expect(await getExportsOfFiles(request)).toEqual({
      [testFiles.dictExportFile]: [
        { type: 'named', name: 'add' },
        { type: 'named', name: 'sub' },
      ],
    });
  });

  test('empty ts file works with empty tsconfig', async () => {
    const request = { filepaths: [testFiles.emptyFile], tsconfig: testFiles.emptyTsconfig };
    expect(await getExportsOfFiles(request)).toEqual({
      [testFiles.emptyFile]: []
    });
  });

  test('`export const` shows up in export list', async () => {
    const request = { filepaths: [testFiles.constExportFile] };
    expect(await getExportsOfFiles(request)).toEqual({
      [testFiles.constExportFile]: [{
        type: 'named', name: 'isEven', range: {
          start: { line: 0, column: 13 },
          end: { line: 2, column: 1 }
        }
      }]
    });
  })
});
