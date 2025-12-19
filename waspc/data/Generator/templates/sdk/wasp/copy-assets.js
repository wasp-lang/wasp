import * as fs from "node:fs/promises";
import * as path from "node:path";

const base = path.resolve(import.meta.dirname);

// TODO: interpolate through Haskell?
const globs = ["./user-core/auth/forms/**/*.css"];

for await (const file of fs.glob(globs, { cwd: base })) {
  const src = path.join(base, file);
  const dest = path.join(base, "dist", file);

  await fs.mkdir(path.dirname(dest), { recursive: true });
  await fs.copyFile(src, dest);
}
