import fs from "fs";
import os from "node:os";
import path from "node:path";

import { afterEach, beforeEach, describe, expect, test } from "vitest";

import {
  WaspCliExe,
  WaspProjectDir,
} from "../../../src/common/brandedTypes.js";
import {
  getClientBuildArtefactsDir,
  getClientSpaFallbackFileName,
  getServerBuildArtefactsDir,
} from "../../../src/common/waspProject.js";
import {
  createServerDeployStagingDir,
  DeployBoundary,
  makeClientVercelJsonContents,
  makeServerVercelJsonContents,
  patchPrismaSchemaForVercel,
  runDeploy,
  SERVER_BUILD_COMMAND,
  SERVER_ENTRYPOINT_SHIM,
  VercelDeployCmdOptions,
} from "../../../src/providers/vercel/deploy.js";
import { VercelCli } from "../../../src/providers/vercel/VercelCli.js";

const SPA_FALLBACK_0_22 = ["index", "html"].join("."); // avoid the literal in src greps
const SPA_FALLBACK_MAIN = ["200", "html"].join(".");

// A pristine Wasp-0.22-style generated schema: no binaryTargets, no directUrl.
const PRISTINE_SCHEMA = `datasource db {
  provider = "postgresql"
  url      = env("DATABASE_URL")
}

generator client {
  provider = "prisma-client-js"
}

model Note {
  id   Int    @id @default(autoincrement())
  text String
}
`;

// A generated schema that already carries (Docker-oriented) binaryTargets.
const SCHEMA_WITH_OTHER_TARGETS = `datasource db {
  provider = "postgresql"
  url      = env("DATABASE_URL")
}

generator client {
  provider      = "prisma-client-js"
  binaryTargets = ["native", "linux-musl"]
}
`;

const PRISTINE_PACKAGE_JSON = JSON.stringify({
  name: "testapp",
  type: "module",
  workspaces: [".wasp/out/*", ".wasp/out/sdk/wasp"],
});

describe("server entrypoint shim", () => {
  test("is the fixed 2-line template: express marker + bundled server import", () => {
    const lines = SERVER_ENTRYPOINT_SHIM.trim().split("\n");
    expect(lines).toHaveLength(2);
    // Inert marker so Vercel's content-based Express detection engages.
    expect(lines[0]).toContain("import express from 'express'");
    // Loads the rollup-bundled, unmodified Wasp server from the mirrored
    // Docker layout (build output nested under .wasp/out/).
    expect(lines[1]).toBe("import './.wasp/out/server/bundle/server.js'");
  });

  test("never inspects user code (contains no placeholders)", () => {
    expect(SERVER_ENTRYPOINT_SHIM).not.toMatch(/[{}<>]|\$\{/);
  });
});

describe("server vercel.json generation", () => {
  const parsed = JSON.parse(makeServerVercelJsonContents()) as Record<
    string,
    string
  >;

  test("declares the express framework and plain npm install", () => {
    expect(parsed.framework).toBe("express");
    expect(parsed.installCommand).toBe("npm install");
  });

  test("buildCommand chains prisma generate -> migrate deploy on DIRECT_URL -> rollup", () => {
    expect(parsed.buildCommand).toBe(SERVER_BUILD_COMMAND);
    const generateIdx = parsed.buildCommand.indexOf("npx prisma generate");
    const migrateIdx = parsed.buildCommand.indexOf(
      "DATABASE_URL=$DIRECT_URL npx prisma migrate deploy",
    );
    const rollupIdx = parsed.buildCommand.indexOf(
      "npx rollup --config --silent",
    );
    expect(generateIdx).toBeGreaterThanOrEqual(0);
    expect(migrateIdx).toBeGreaterThan(generateIdx);
    expect(rollupIdx).toBeGreaterThan(migrateIdx);
  });

  test("buildCommand runs inside the nested build-output dir of the mirrored layout", () => {
    expect(parsed.buildCommand.startsWith("cd .wasp/out && ")).toBe(true);
  });

  test("build steps address the generated schema explicitly", () => {
    const schemaFlags = parsed.buildCommand.match(
      /--schema=db\/schema\.prisma/g,
    );
    expect(schemaFlags).toHaveLength(2);
  });

  test("skips tsc --build (broken references under the flattened deploy root)", () => {
    expect(parsed.buildCommand).not.toContain("tsc");
    expect(parsed.buildCommand).not.toContain("npm run bundle");
  });
});

describe("client SPA-rewrite vercel.json generation", () => {
  test("rewrites every route to the given SPA fallback file", () => {
    const parsed = JSON.parse(makeClientVercelJsonContents(SPA_FALLBACK_0_22));
    expect(parsed).toEqual({
      rewrites: [
        { source: "/(.*)", destination: `/${SPA_FALLBACK_0_22}` },
      ],
    });
  });

  test("uses whatever fallback filename it is given (no hardcoding)", () => {
    const parsed = JSON.parse(makeClientVercelJsonContents(SPA_FALLBACK_MAIN));
    expect(parsed.rewrites[0].destination).toBe(`/${SPA_FALLBACK_MAIN}`);
  });
});

describe("patchPrismaSchemaForVercel", () => {
  test("adds Vercel-compatible binaryTargets to the generator client block", () => {
    const patched = patchPrismaSchemaForVercel(PRISTINE_SCHEMA);
    const generatorBlock = patched.match(/generator\s+client\s*\{[^}]*\}/)![0];
    expect(generatorBlock).toContain(
      'binaryTargets = ["native", "rhel-openssl-3.0.x", "debian-openssl-3.0.x"]',
    );
  });

  test("replaces pre-existing binaryTargets instead of duplicating them", () => {
    const patched = patchPrismaSchemaForVercel(SCHEMA_WITH_OTHER_TARGETS);
    expect(patched).not.toContain("linux-musl");
    expect(patched.match(/binaryTargets/g)).toHaveLength(1);
  });

  test("adds directUrl = env(\"DIRECT_URL\") to the datasource block", () => {
    const patched = patchPrismaSchemaForVercel(PRISTINE_SCHEMA);
    const datasourceBlock = patched.match(/datasource\s+\w+\s*\{[^}]*\}/)![0];
    expect(datasourceBlock).toContain('directUrl = env("DIRECT_URL")');
    // The pooled runtime URL stays untouched.
    expect(datasourceBlock).toContain('url      = env("DATABASE_URL")');
  });

  test("is idempotent (safe to run on an already patched schema)", () => {
    const once = patchPrismaSchemaForVercel(PRISTINE_SCHEMA);
    const twice = patchPrismaSchemaForVercel(once);
    expect(twice).toBe(once);
  });

  test("leaves user models untouched", () => {
    const patched = patchPrismaSchemaForVercel(PRISTINE_SCHEMA);
    expect(patched).toContain("model Note {");
  });

  test("throws a clear error when the schema has no generator client block", () => {
    expect(() => patchPrismaSchemaForVercel("datasource db {\n}\n")).toThrow(
      /generator/,
    );
  });
});

// ---------------------------------------------------------------------------
// Filesystem-level tests (temp dirs standing in for a `wasp build` output).
// ---------------------------------------------------------------------------

let tempWaspProjectDir: WaspProjectDir;

// Mirrors the Wasp 0.22 build output layout that the generated Dockerfile
// consumes: user code copy in src/, root package.json + tsconfig.json, and
// the framework dirs (server/, sdk/, libs/, db/).
function makeFakeWaspBuildOutput(): string {
  const outDir = getServerBuildArtefactsDir(tempWaspProjectDir);
  fs.mkdirSync(path.join(outDir, "db"), { recursive: true });
  fs.writeFileSync(path.join(outDir, "db", "schema.prisma"), PRISTINE_SCHEMA);
  fs.writeFileSync(path.join(outDir, "package.json"), PRISTINE_PACKAGE_JSON);
  fs.writeFileSync(path.join(outDir, "package-lock.json"), "{}");
  fs.writeFileSync(path.join(outDir, "tsconfig.json"), "{}");
  fs.mkdirSync(path.join(outDir, "src"), { recursive: true });
  fs.writeFileSync(path.join(outDir, "src", "apis.ts"), "export const x = 1;");
  fs.mkdirSync(path.join(outDir, "server", "node_modules", "junk"), {
    recursive: true,
  });
  fs.writeFileSync(path.join(outDir, "server", "package.json"), "{}");
  fs.mkdirSync(path.join(outDir, "sdk", "wasp"), { recursive: true });
  fs.writeFileSync(path.join(outDir, "sdk", "wasp", "package.json"), "{}");
  fs.mkdirSync(path.join(outDir, "libs"), { recursive: true });
  fs.writeFileSync(path.join(outDir, "libs", "some-lib.tgz"), "");
  return outDir;
}

function makeFakeClientBuildOutput(spaFallbackFileName: string): string {
  const buildDir = getClientBuildArtefactsDir(tempWaspProjectDir);
  fs.mkdirSync(buildDir, { recursive: true });
  fs.writeFileSync(
    path.join(buildDir, spaFallbackFileName),
    '<html><div id="root"></div></html>',
  );
  return buildDir;
}

beforeEach(() => {
  tempWaspProjectDir = fs.mkdtempSync(
    path.join(os.tmpdir(), "wasp-vercel-deploy-test-"),
  ) as WaspProjectDir;
});

afterEach(() => {
  fs.rmSync(tempWaspProjectDir, { recursive: true, force: true });
});

describe("getClientSpaFallbackFileName", () => {
  test("resolves the Wasp 0.22 fallback when only that file exists", () => {
    makeFakeClientBuildOutput(SPA_FALLBACK_0_22);
    expect(getClientSpaFallbackFileName(tempWaspProjectDir)).toBe(
      SPA_FALLBACK_0_22,
    );
  });

  test("resolves the Wasp main (0.25-dev) fallback when that file exists", () => {
    makeFakeClientBuildOutput(SPA_FALLBACK_MAIN);
    expect(getClientSpaFallbackFileName(tempWaspProjectDir)).toBe(
      SPA_FALLBACK_MAIN,
    );
  });

  test("prefers the dedicated SPA fallback file when both exist", () => {
    makeFakeClientBuildOutput(SPA_FALLBACK_0_22);
    makeFakeClientBuildOutput(SPA_FALLBACK_MAIN);
    expect(getClientSpaFallbackFileName(tempWaspProjectDir)).toBe(
      SPA_FALLBACK_MAIN,
    );
  });

  test("throws when the client build produced no known fallback file", () => {
    fs.mkdirSync(getClientBuildArtefactsDir(tempWaspProjectDir), {
      recursive: true,
    });
    expect(() => getClientSpaFallbackFileName(tempWaspProjectDir)).toThrow();
  });
});

describe("createServerDeployStagingDir", () => {
  let stagingDir: string;

  afterEach(() => {
    if (stagingDir !== undefined) {
      fs.rmSync(stagingDir, { recursive: true, force: true });
    }
  });

  test("mirrors the Docker layout the generated relative imports expect", () => {
    // Framework code in .wasp/out/server imports user code via relative
    // paths that assume the project layout (src/ at the root, framework
    // dirs nested under .wasp/out/) - same reason the generated Dockerfile
    // mirrors this structure.
    stagingDir = createServerDeployStagingDir(makeFakeWaspBuildOutput());

    expect(fs.existsSync(path.join(stagingDir, "src", "apis.ts"))).toBe(true);
    expect(fs.existsSync(path.join(stagingDir, "package.json"))).toBe(true);
    expect(fs.existsSync(path.join(stagingDir, "package-lock.json"))).toBe(
      true,
    );
    expect(fs.existsSync(path.join(stagingDir, "tsconfig.json"))).toBe(true);
    for (const dir of ["server", "sdk", "libs", "db"]) {
      expect(
        fs.existsSync(path.join(stagingDir, ".wasp", "out", dir)),
      ).toBe(true);
    }
  });

  test("keeps the generated package.json (workspaces globs) verbatim", () => {
    stagingDir = createServerDeployStagingDir(makeFakeWaspBuildOutput());
    const packageJson = JSON.parse(
      fs.readFileSync(path.join(stagingDir, "package.json"), "utf-8"),
    );
    // The generated globs are relative to the mirrored layout root, exactly
    // like in the Docker image - no rewrite needed or wanted.
    expect(packageJson.workspaces).toEqual([
      ".wasp/out/*",
      ".wasp/out/sdk/wasp",
    ]);
  });

  test("writes the entrypoint shim and vercel.json at the staging root", () => {
    stagingDir = createServerDeployStagingDir(makeFakeWaspBuildOutput());
    expect(fs.readFileSync(path.join(stagingDir, "server.js"), "utf-8")).toBe(
      SERVER_ENTRYPOINT_SHIM,
    );
    expect(
      fs.readFileSync(path.join(stagingDir, "vercel.json"), "utf-8"),
    ).toBe(makeServerVercelJsonContents());
  });

  test("patches the staged Prisma schema (binaryTargets + directUrl)", () => {
    stagingDir = createServerDeployStagingDir(makeFakeWaspBuildOutput());
    const schema = fs.readFileSync(
      path.join(stagingDir, ".wasp", "out", "db", "schema.prisma"),
      "utf-8",
    );
    expect(schema).toContain("rhel-openssl-3.0.x");
    expect(schema).toContain('directUrl = env("DIRECT_URL")');
  });

  test("leaves the original build output untouched", () => {
    const outDir = makeFakeWaspBuildOutput();
    stagingDir = createServerDeployStagingDir(outDir);
    expect(
      fs.readFileSync(path.join(outDir, "db", "schema.prisma"), "utf-8"),
    ).toBe(PRISTINE_SCHEMA);
    expect(fs.existsSync(path.join(outDir, "server.js"))).toBe(false);
    expect(fs.existsSync(path.join(outDir, "vercel.json"))).toBe(false);
  });

  test("does not copy node_modules into the staging dir", () => {
    stagingDir = createServerDeployStagingDir(makeFakeWaspBuildOutput());
    expect(
      fs.existsSync(
        path.join(stagingDir, ".wasp", "out", "server", "node_modules"),
      ),
    ).toBe(false);
  });

  test("throws a clear error when the build output looks incomplete", () => {
    const outDir = makeFakeWaspBuildOutput();
    fs.rmSync(path.join(outDir, "server"), { recursive: true });
    expect(() => createServerDeployStagingDir(outDir)).toThrow(/server/);
  });
});

// ---------------------------------------------------------------------------
// Orchestration tests: runDeploy against an in-memory VercelCli fake and a
// fake wasp-build/client-build boundary (no processes are ever spawned).
// ---------------------------------------------------------------------------

class FakeVercelCli implements VercelCli {
  existingProjects = new Set<string>();
  events: string[] = [];
  linkedDirs = new Map<string, string>(); // dir -> project name
  deployedDirs: string[] = [];
  // Filenames present in each deployed dir, snapshotted at deploy time (the
  // server staging dir is ephemeral - it is cleaned up right after deploy).
  deployedDirFiles = new Map<string, string[]>(); // project name -> files

  async doesProjectExist(projectName: string): Promise<boolean> {
    return this.existingProjects.has(projectName);
  }

  async linkProjectToDir(projectName: string, dir: string): Promise<void> {
    this.linkedDirs.set(path.normalize(dir), projectName);
    this.events.push(`link:${projectName}`);
  }

  async deployToProd(linkedProjectDir: string): Promise<string> {
    const projectName = this.linkedDirs.get(path.normalize(linkedProjectDir));
    if (projectName === undefined) {
      throw new Error(`deploy from unlinked dir: ${linkedProjectDir}`);
    }
    this.deployedDirs.push(path.normalize(linkedProjectDir));
    this.deployedDirFiles.set(projectName, fs.readdirSync(linkedProjectDir));
    this.events.push(`deploy:${projectName}`);
    return `https://${projectName}-deadbeef.vercel.app`;
  }

  async createProject(): Promise<void> {
    throw new Error("deploy must never create projects (setup's job)");
  }
  async setEnvVar(): Promise<void> {
    throw new Error("deploy must never set env vars (setup's job)");
  }
  async pullEnvVars(): Promise<Record<string, string>> {
    throw new Error("deploy must never pull env vars");
  }
  async isIntegrationInstalled(): Promise<boolean> {
    throw new Error("deploy must never touch integrations");
  }
  async addIntegrationResource(): Promise<{ success: boolean; output: string }> {
    throw new Error("deploy must never touch integrations");
  }
  async connectIntegrationResource(): Promise<{
    success: boolean;
    output: string;
  }> {
    throw new Error("deploy must never touch integrations");
  }
}

interface FakeBoundary {
  boundary: DeployBoundary;
  waspBuilds: number;
  clientBuildServerUrls: string[];
}

function makeFakeBoundary(
  spaFallbackFileName: string = SPA_FALLBACK_0_22,
): FakeBoundary {
  const fake: FakeBoundary = {
    waspBuilds: 0,
    clientBuildServerUrls: [],
    boundary: {
      ensureWaspProjectIsBuilt: async () => {
        fake.waspBuilds += 1;
        makeFakeWaspBuildOutput();
      },
      buildClient: async (serverUrl: string) => {
        fake.clientBuildServerUrls.push(serverUrl);
        return makeFakeClientBuildOutput(spaFallbackFileName);
      },
    },
  };
  return fake;
}

function makeOptions(): VercelDeployCmdOptions {
  return {
    waspExe: "wasp" as WaspCliExe,
    waspProjectDir: tempWaspProjectDir,
    vercelExe: "vercel",
  };
}

describe("runDeploy", () => {
  let cli: FakeVercelCli;

  beforeEach(() => {
    cli = new FakeVercelCli();
    cli.existingProjects.add("my-app-server");
    cli.existingProjects.add("my-app-client");
  });

  test("builds once, then links and deploys server before client", async () => {
    const fake = makeFakeBoundary();
    await runDeploy(cli, "my-app", makeOptions(), fake.boundary);

    expect(fake.waspBuilds).toBe(1);
    expect(cli.events).toEqual([
      "link:my-app-server",
      "deploy:my-app-server",
      "link:my-app-client",
      "deploy:my-app-client",
    ]);
  });

  test("deploys the server from a staged dir carrying the shim, config and mirrored layout", async () => {
    const fake = makeFakeBoundary();
    await runDeploy(cli, "my-app", makeOptions(), fake.boundary);

    const deployedServerFiles = cli.deployedDirFiles.get("my-app-server");
    expect(deployedServerFiles).toContain("server.js");
    expect(deployedServerFiles).toContain("vercel.json");
    expect(deployedServerFiles).toContain("package.json");
    expect(deployedServerFiles).toContain("src");
    expect(deployedServerFiles).toContain(".wasp");
  });

  test("cleans the ephemeral server staging dir up after deploying", async () => {
    const fake = makeFakeBoundary();
    await runDeploy(cli, "my-app", makeOptions(), fake.boundary);
    expect(fs.existsSync(cli.deployedDirs[0])).toBe(false);
  });

  test("builds the client with the predicted server URL, not a placeholder", async () => {
    const fake = makeFakeBoundary();
    await runDeploy(cli, "my-app", makeOptions(), fake.boundary);
    expect(fake.clientBuildServerUrls).toEqual([
      "https://my-app-server.vercel.app",
    ]);
  });

  test("writes the SPA-rewrite vercel.json into the client build dir (resolved fallback)", async () => {
    const fake = makeFakeBoundary(SPA_FALLBACK_0_22);
    await runDeploy(cli, "my-app", makeOptions(), fake.boundary);

    const buildDir = getClientBuildArtefactsDir(tempWaspProjectDir);
    const vercelJson = JSON.parse(
      fs.readFileSync(path.join(buildDir, "vercel.json"), "utf-8"),
    );
    expect(vercelJson.rewrites).toEqual([
      { source: "/(.*)", destination: `/${SPA_FALLBACK_0_22}` },
    ]);
    expect(cli.deployedDirs[1]).toBe(path.normalize(buildDir));
  });

  test("resolves the SPA fallback per build output (wasp main emits a different file)", async () => {
    const fake = makeFakeBoundary(SPA_FALLBACK_MAIN);
    await runDeploy(cli, "my-app", makeOptions(), fake.boundary);

    const vercelJson = JSON.parse(
      fs.readFileSync(
        path.join(getClientBuildArtefactsDir(tempWaspProjectDir), "vercel.json"),
        "utf-8",
      ),
    );
    expect(vercelJson.rewrites[0].destination).toBe(`/${SPA_FALLBACK_MAIN}`);
  });

  test("fails fast (before building) when the projects were never set up", async () => {
    const emptyCli = new FakeVercelCli();
    const fake = makeFakeBoundary();
    await expect(
      runDeploy(emptyCli, "my-app", makeOptions(), fake.boundary),
    ).rejects.toThrow(/setup/);
    expect(fake.waspBuilds).toBe(0);
    expect(emptyCli.events).toEqual([]);
  });

  test("rejects an invalid app name before doing anything", async () => {
    const fake = makeFakeBoundary();
    await expect(
      runDeploy(cli, "My Bad App", makeOptions(), fake.boundary),
    ).rejects.toThrow();
    expect(fake.waspBuilds).toBe(0);
  });

  test("re-running deploy against already-deployed projects deploys again (idempotent, no setup)", async () => {
    const fake = makeFakeBoundary();
    await runDeploy(cli, "my-app", makeOptions(), fake.boundary);
    await runDeploy(cli, "my-app", makeOptions(), fake.boundary);
    expect(
      cli.events.filter((e) => e === "deploy:my-app-server"),
    ).toHaveLength(2);
    expect(
      cli.events.filter((e) => e === "deploy:my-app-client"),
    ).toHaveLength(2);
  });
});
