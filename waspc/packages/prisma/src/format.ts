import * as tmp from 'tmp-promise'
import fs from 'fs';
import { spawn } from 'child_process';
import stripAnsi from 'strip-ansi';
import util from 'util'

// tmp-promise by default closes and unlinks the files on process exit.

export async function formatSchema(schemaPsl : string) : Promise<{ formattedSchemaPsl: string, stderr: string, exitCode: number }> {
  const tmpPrismaSchemaFile = await tmp.file({ prefix: 'prisma-', postfix: '.schema' });
  await util.promisify(fs.writeFile)(tmpPrismaSchemaFile.path, schemaPsl, 'utf8');

  let stderr = '';
  let exitCode: number;
  {
    const process = spawn("npx", ["prisma", "format", "--schema", tmpPrismaSchemaFile.path], {});
    process.stderr.on('data', (data) => { stderr += data; });
    exitCode = await new Promise((resolve, reject) => {
      process.on('error', reject);
      process.on('close', (code) => { resolve(code || 0); });
    });
  }

  const formattedSchemaPsl = await util.promisify(fs.readFile)(tmpPrismaSchemaFile.path, 'utf8');
  await tmpPrismaSchemaFile.cleanup();

  return {
    formattedSchemaPsl,
    stderr: stripAnsi(stderr),
    exitCode: exitCode,
  };
}