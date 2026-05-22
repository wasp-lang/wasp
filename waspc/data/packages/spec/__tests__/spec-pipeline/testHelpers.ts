import { mkdirSync, mkdtempSync, realpathSync, writeFileSync } from "fs";
import { tmpdir } from "os";
import { dirname, join } from "path";

export function makeTempProject(prefix: string): string {
  // Jiti reports loaded files using canonical paths, so use a canonical temp root too.
  const tempDir = mkdtempSync(join(realpathSync(tmpdir()), prefix));
  writePackageJson(tempDir);
  return tempDir;
}

export function writePackageJson(tempDir: string): void {
  writeFileSync(
    join(tempDir, "package.json"),
    JSON.stringify({ type: "module" }),
  );
}

export function writeNodePackage(
  tempDir: string,
  packageName: string,
  files: { js: string; dts: string },
): void {
  const packageDir = join(tempDir, "node_modules", packageName);
  mkdirSync(packageDir, { recursive: true });
  writeFileSync(
    join(packageDir, "package.json"),
    JSON.stringify({
      type: "module",
      exports: "./index.js",
      types: "./index.d.ts",
    }),
  );
  writeFileSync(join(packageDir, "index.js"), files.js);
  writeFileSync(join(packageDir, "index.d.ts"), files.dts);
}

export function writeProjectFile(
  tempDir: string,
  relativePath: string,
  sourceText: string,
): void {
  const filePath = join(tempDir, relativePath);
  mkdirSync(dirname(filePath), { recursive: true });
  writeFileSync(filePath, sourceText, "utf8");
}

export function writeUserSourceFiles(tempDir: string): void {
  mkdirSync(join(tempDir, "src"));
  writeFileSync(
    join(tempDir, "src", "MainPage.ts"),
    `export default function MainPage() { return null; }\n`,
  );
  writeFileSync(
    join(tempDir, "src", "operations.ts"),
    [
      `export async function getTasks() { return []; }`,
      `export async function archive() { return null; }`,
      `export async function logout() { return null; }`,
      ``,
    ].join("\n"),
  );
  writeFileSync(
    join(tempDir, "src", "adminOperations.ts"),
    `export async function archive() { return null; }\n`,
  );
}

export function writeTsConfig(tsconfigPath: string, include: string): void {
  writeFileSync(
    tsconfigPath,
    JSON.stringify({
      compilerOptions: {
        target: "ES2022",
        module: "ESNext",
        moduleResolution: "bundler",
        jsx: "preserve",
        strict: true,
        allowJs: true,
        noEmit: true,
      },
      include: [include, "**/*.wasp.ts"],
    }),
  );
}
