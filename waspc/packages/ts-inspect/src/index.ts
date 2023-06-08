import { getExportsOfFiles } from "./exports.js";

interface ExportsRequest {
  filenames: string[],
  tsconfig?: string,
}

async function readStdin(): Promise<string> {
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

async function main() {
  const inputStr = await readStdin();
  const input = JSON.parse(inputStr);
  if (!Array.isArray(input)) {
    throw new Error('Expected JSON array from stdin');
  }
  const requests: ExportsRequest[] = input;

  let exports = {};
  for (let request of requests) {
    const newExports = await getExportsOfFiles(request.filenames, request.tsconfig);
    exports = { ...exports, ...newExports };
  }
  console.log(JSON.stringify(exports));
}

main().catch((err) => console.error(err));
