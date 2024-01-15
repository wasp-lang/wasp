import * as path from 'path';
import fs from 'fs';
import util from 'util'
import { formatSchema } from '../src/format';

/**
 * Get an absolute path to a test file.
 * @param filename Name of test file inside __dirname/formatTests directory.
 */
function testFilePath(filename: string): string {
  return path.join(__dirname, 'formatTests', filename);
}

const testSchemaPsls = {
  simpleFormatted: testFilePath('simple/formatted.schema'),
  simpleUnformatted: testFilePath('simple/unformatted.schema'),
  simplePartialRelation: testFilePath('simple/partial-relation.schema'),
  simplePartialRelationFixed: testFilePath('simple/partial-relation-fixed.schema'),
  simpleBroken: testFilePath('simple/broken.schema'),
};

async function fetchTestSchemaPsl(testSchemaName: keyof typeof testSchemaPsls): Promise<string> {
  return util.promisify(fs.readFile)(testSchemaPsls[testSchemaName], 'utf8');
}

describe('format.ts', () => {
  describe('for a simple schema with User and Task models', () => {
    let formattedSchemaPsl: string;
    beforeAll(async () => {
      formattedSchemaPsl = await fetchTestSchemaPsl("simpleFormatted");
    });

    test('already formatted prisma schema is returned as it was', async () => {
      expect(await formatSchema(formattedSchemaPsl)).toEqual({
        formattedSchemaPsl, stderr: '', exitCode: 0,
      });
    });

    test('unformatted prisma schema is correctly formatted', async () => {
      expect(await formatSchema(await fetchTestSchemaPsl("simpleUnformatted"))).toEqual({
        formattedSchemaPsl, stderr: '', exitCode: 0,
      });
    });

    test('prisma schema with partial relation is correctly fixed', async () => {
      expect(await formatSchema(await fetchTestSchemaPsl("simplePartialRelation"))).toEqual({
        formattedSchemaPsl: await fetchTestSchemaPsl("simplePartialRelationFixed"), stderr: '', exitCode: 0,
      });
    });

    test('if prisma schema is broken, errors are reported and exit code is not 0', async () => {
      const result = await formatSchema(await fetchTestSchemaPsl("simpleBroken"))
      expect(result.exitCode).not.toEqual(0);
      expect(result.stderr).toContain('Error: Prisma schema validation');
    });
  })
});

