import { readFileSync } from "node:fs";
import { isAbsolute, join } from "node:path";

export function discoverWaspModulePackages(appRootDir) {
  const appPackageJson = readPackageJson(join(appRootDir, "package.json"));

  return Object.keys(appPackageJson.dependencies ?? {}).filter(
    (packageName) => {
      const packageJson = readPackageJson(
        join(appRootDir, "node_modules", packageName, "package.json"),
      );
      return isObject(packageJson.wasp) && isObject(packageJson.wasp.module);
    },
  );
}

export function shouldExternalize(importPath, packagesToBundle) {
  return (
    isPackageImport(importPath) &&
    !packagesToBundle.has(getPackageName(importPath))
  );
}

function isPackageImport(importPath) {
  return (
    !importPath.startsWith(".") &&
    !importPath.startsWith("\0") &&
    !importPath.startsWith("#") &&
    !isAbsolute(importPath)
  );
}

function getPackageName(importPath) {
  const [firstPart, secondPart] = importPath.split("/");
  return firstPart.startsWith("@") ? `${firstPart}/${secondPart}` : firstPart;
}

function readPackageJson(packageJsonPath) {
  return JSON.parse(readFileSync(packageJsonPath, "utf8"));
}

function isObject(value) {
  return typeof value === "object" && value !== null && !Array.isArray(value);
}
