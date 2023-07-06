import process from 'process';
import { formatSchema } from './format.js';

async function main() {
  const args = process.argv.slice(2);
  switch (args[0]) {
    case 'format':
      const schemaPsl = await readWholeStdin();
      const { formattedSchemaPsl, stderr, exitCode } = await formatSchema(schemaPsl);
      console.log(JSON.stringify({ formattedSchemaPsl, errors: exitCode === 0 ? undefined : stderr }));
      break;
    default:
      console.log("Specify a command!");
  }
}

async function readWholeStdin(): Promise<string> {
  return new Promise((resolve, reject) => {
    let chunks = '';
    process.stdin.on('data', (data) => {
      chunks += data;
    });
    process.stdin.on('end', () => resolve(chunks));
    process.stdin.on('close', () => resolve(chunks));
    process.stdin.on('error', (err) => reject(err));
  });
}

main().catch((err) => { console.error(err); process.exit(1); });
