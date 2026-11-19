import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { afterEach, beforeEach, describe, expect, it } from "vitest";
import {
  getImportedPackageName,
  isWaspDependencyImport,
  linkWaspDependencies,
} from "../src/node/sdk/dependencyLinks";

describe("linkWaspDependencies", () => {
  let projectDir: string;
  let generatedAppDir: string;
  let waspPackageDir: string;

  beforeEach(() => {
    // macOS's temp dir is a symlink, and the links point at real paths.
    projectDir = fs.realpathSync(
      fs.mkdtempSync(path.join(os.tmpdir(), "wasp-dependency-links-")),
    );
    generatedAppDir = path.join(projectDir, ".wasp/out");
    // Like pnpm, which keeps a package and its dependencies apart from the
    // project's own `node_modules`.
    waspPackageDir = path.join(
      projectDir,
      "node_modules/.store/wasp/node_modules/@wasp/wasp",
    );
    writePackage(waspPackageDir, {
      name: "@wasp/wasp",
      dependencies: { "@wasp/sdk-core": "1.0.0" },
      peerDependencies: { react: "^19.0.0", lucia: "^3.0.0", prisma: "6.0.0" },
    });
  });

  afterEach(() => {
    fs.rmSync(projectDir, { recursive: true, force: true });
  });

  const link = () =>
    linkWaspDependencies({ waspPackageDir, projectDir, generatedAppDir });
  const resolveLink = (linkPathInLinksDir: string) =>
    fs.realpathSync(
      path.join(generatedAppDir, "node_modules", linkPathInLinksDir),
    );
  const storeDirOf = (packageName: string) =>
    path.join(projectDir, "node_modules/.store/wasp/node_modules", packageName);
  const projectDirOf = (packageName: string) =>
    path.join(projectDir, "node_modules", packageName);

  it("links a package from next to the Wasp package", () => {
    writePackage(storeDirOf("@wasp/sdk-core"), { name: "@wasp/sdk-core" });

    link();

    expect(resolveLink("@wasp/sdk-core")).toBe(storeDirOf("@wasp/sdk-core"));
  });

  it("prefers the copy that the project lists itself", () => {
    writePackage(storeDirOf("react"), { name: "react" });
    writePackage(projectDirOf("react"), { name: "react" });

    link();

    expect(resolveLink("react")).toBe(projectDirOf("react"));
  });

  it("skips an optional peer dependency that isn't installed", () => {
    link();

    expect(
      fs.existsSync(path.join(generatedAppDir, "node_modules/lucia")),
    ).toBe(false);
  });

  it("links the executables of a package", () => {
    writePackage(storeDirOf("prisma"), {
      name: "prisma",
      bin: { prisma: "build/index.js" },
    });

    link();

    expect(resolveLink(".bin/prisma")).toBe(
      path.join(storeDirOf("prisma"), "build/index.js"),
    );
  });

  it("removes the link of a package that is gone", () => {
    writePackage(projectDirOf("lucia"), { name: "lucia" });
    link();
    fs.rmSync(projectDirOf("lucia"), { recursive: true });

    link();

    expect(
      fs.readdirSync(path.join(generatedAppDir, "node_modules")),
    ).not.toContain("lucia");
  });

  it("leaves an up-to-date link alone", () => {
    writePackage(storeDirOf("@wasp/sdk-core"), { name: "@wasp/sdk-core" });
    link();
    const linkPath = path.join(generatedAppDir, "node_modules/@wasp/sdk-core");
    const createdAt = fs.lstatSync(linkPath).birthtimeMs;

    link();

    expect(fs.lstatSync(linkPath).birthtimeMs).toBe(createdAt);
  });

  describe("isWaspDependencyImport", () => {
    beforeEach(() => {
      writePackage(storeDirOf("@wasp/sdk-core"), { name: "@wasp/sdk-core" });
      link();
    });

    it.each(["@wasp/sdk-core", "@wasp/sdk-core/node/sdk"])(
      "is true for %s, which is of a linked package",
      (importSpecifier) => {
        expect(isWaspDependencyImport(generatedAppDir, importSpecifier)).toBe(
          true,
        );
      },
    );

    it.each(["lodash", "@wasp/other", "./@wasp/sdk-core", "/@wasp/sdk-core"])(
      "is false for %s",
      (importSpecifier) => {
        expect(isWaspDependencyImport(generatedAppDir, importSpecifier)).toBe(
          false,
        );
      },
    );
  });
});

describe("getImportedPackageName", () => {
  it.each([
    ["zod", "zod"],
    ["zod/v4", "zod"],
    ["@wasp/auth", "@wasp/auth"],
    ["@wasp/auth/browser", "@wasp/auth"],
    ["./zod", undefined],
    ["/abs/zod", undefined],
  ])("of %s is %s", (importSpecifier, packageName) => {
    expect(getImportedPackageName(importSpecifier)).toBe(packageName);
  });
});

function writePackage(
  packageDir: string,
  packageJson: Record<string, unknown>,
): void {
  fs.mkdirSync(packageDir, { recursive: true });
  fs.writeFileSync(
    path.join(packageDir, "package.json"),
    JSON.stringify(packageJson),
  );
  const { bin } = packageJson;
  if (typeof bin === "object" && bin !== null) {
    for (const binFile of Object.values(bin)) {
      const binFilePath = path.join(packageDir, binFile);
      fs.mkdirSync(path.dirname(binFilePath), { recursive: true });
      fs.writeFileSync(binFilePath, "");
    }
  }
}
