import {
  existsSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import path from "node:path";
import { expect, test } from "vitest";
import { buildModule } from "../src/index.js";

test("a source-free build removes stale output and emits spec types", async () => {
  const moduleDir = scaffoldModule();
  const staleOutputPath = path.join(moduleDir, "dist", "stale.js");
  mkdirSync(path.dirname(staleOutputPath), { recursive: true });
  writeFileSync(staleOutputPath, "stale");

  try {
    await buildModule(moduleDir);

    expect(existsSync(staleOutputPath)).toBe(false);
    expect(readFileSync(specDeclarationPath(moduleDir), "utf8")).toContain(
      "prefix: string",
    );
  } finally {
    rmSync(moduleDir, { recursive: true, force: true });
  }
});

function scaffoldModule(): string {
  const moduleDir = mkdtempSync(
    path.join(tmpdir(), "wasp-module-builder-test-"),
  );
  writeFileSync(
    path.join(moduleDir, "package.json"),
    JSON.stringify({ name: "test-module", type: "module" }),
  );
  writeFileSync(
    path.join(moduleDir, "tsconfig.src.json"),
    JSON.stringify({
      compilerOptions: {
        target: "ES2022",
        module: "ESNext",
        moduleResolution: "Bundler",
        strict: true,
        skipLibCheck: true,
        noEmit: true,
      },
    }),
  );
  writeFileSync(
    path.join(moduleDir, "tsconfig.wasp.json"),
    JSON.stringify({
      compilerOptions: {
        target: "ES2022",
        module: "ESNext",
        moduleResolution: "Bundler",
        strict: true,
        skipLibCheck: true,
        noEmit: true,
      },
    }),
  );
  writeModuleSpec(moduleDir, "string");

  return moduleDir;
}

function writeModuleSpec(moduleDir: string, prefixType: string): void {
  writeFileSync(
    path.join(moduleDir, "module.wasp.ts"),
    `export default function getModuleSpec(options: { prefix: ${prefixType} }) { return options; }\n`,
  );
}

function specDeclarationPath(moduleDir: string): string {
  return path.join(moduleDir, "dist", "spec.d.ts");
}
