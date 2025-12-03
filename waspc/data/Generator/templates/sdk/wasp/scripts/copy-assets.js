import * as fs from "node:fs/promises"
import * as path from "node:path"

const base = path.resolve(import.meta.dirname, "..");

const jsExtensions = ['.js', '.ts', '.jsx', '.tsx', '.d.ts', '.map', '.json', '.tsbuildinfo'];

for await (const file of fs.glob("./**/*.*", { cwd: base })) {
  if (file.startsWith('dist/')) {
    continue;
  }

  const ext = path.extname(file);
  if (jsExtensions.includes(ext)) {
    continue;
  }

  const src = path.join(base, file);
  const dest = path.join(base, "dist", file);
  console.log(`Copying ${src} to ${dest}`);

  await fs.mkdir(path.dirname(dest), { recursive: true });
  await fs.copyFile(src, dest);
}
