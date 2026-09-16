// tsc does not carry stylesheets; copy every .css under src/ next to its
// emitted module so CSS module imports resolve from dist/.
import { cpSync, readdirSync, statSync } from "node:fs";
import { join } from "node:path";

function walk(dir, visit) {
  for (const entry of readdirSync(dir)) {
    const path = join(dir, entry);
    if (statSync(path).isDirectory()) walk(path, visit);
    else visit(path);
  }
}

walk("src", (path) => {
  if (path.endsWith(".css")) {
    cpSync(path, path.replace(/^src/, "dist"));
  }
});
