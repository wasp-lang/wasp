import fs from "fs";
import os from "node:os";
import path from "node:path";

import { WaspCliExe, WaspProjectDir } from "../../common/brandedTypes.js";
import { buildClient } from "../../common/clientApp.js";
import { displayWaspRocketImage, waspSays } from "../../common/terminal.js";
import { ensureWaspProjectIsBuilt } from "../../common/waspBuild.js";
import {
  getClientSpaFallbackFileName,
  getServerBuildArtefactsDir,
} from "../../common/waspProject.js";
import {
  assertVercelAppNameIsValid,
  getClientAppUrl,
  getClientProjectName,
  getServerAppUrl,
  getServerProjectName,
} from "./brandedUrls.js";
import { createVercelCli, VercelCli } from "./VercelCli.js";

export interface VercelDeployCmdOptions {
  waspExe: WaspCliExe;
  waspProjectDir: WaspProjectDir;
  vercelExe: string;
  token?: string;
  scope?: string;
}

/**
 * The process boundaries of the deploy command besides the Vercel CLI
 * itself (`wasp build` and the client Vite build). Unit tests inject fakes
 * here instead of spawning processes.
 */
export interface DeployBoundary {
  ensureWaspProjectIsBuilt: (options: {
    waspProjectDir: WaspProjectDir;
    waspExe: WaspCliExe;
  }) => Promise<unknown>;
  buildClient: (
    serverUrl: string,
    options: { waspProjectDir: WaspProjectDir },
  ) => Promise<string>;
}

const defaultBoundary: DeployBoundary = {
  ensureWaspProjectIsBuilt,
  buildClient,
};

// ---------------------------------------------------------------------------
// Server deploy artifacts (per docs/manual-recipe.md, verified in the m-1
// experiments, plus one correction the recipe's test app could not surface).
// The server path is Vercel's Express zero-config on Fluid compute.
//
// Deploy-root layout: the framework code in `.wasp/out/server` imports the
// user's code with relative paths that assume the project layout (e.g.
// `../../../../../../src/apis`, resolving to `<root>/src/...` only when the
// build output sits nested under `<root>/.wasp/out/`). The generated
// Dockerfile mirrors that structure for exactly this reason, and so do we:
// the deploy root is an ephemeral staging dir shaped like the Docker image
// (src/, package.json, tsconfig.json at the root; server/sdk/libs/db under
// .wasp/out/), rebuilt from the Wasp build output on every deploy.
// ---------------------------------------------------------------------------

/**
 * Vercel's Express zero-config only scans app|index|server.{js,...} at the
 * deploy root, but Wasp's entry lives far below it. This fixed 2-line shim
 * bridges the gap: the express import is an inert marker for Vercel's
 * content-based framework detection, the second import loads the
 * rollup-bundled, unmodified Wasp server (which does its normal
 * `http.createServer(app).listen(process.env.PORT)` startup).
 */
export const SERVER_ENTRYPOINT_SHIM = [
  "import express from 'express' // eslint-disable-line no-unused-vars",
  "import './.wasp/out/server/bundle/server.js'",
  "",
].join("\n");

/**
 * Build order matters: `prisma generate` (schema only, pooled url fine),
 * then `prisma migrate deploy` with DATABASE_URL overridden to the direct
 * (non-pooled) DIRECT_URL for that one sub-step only, then bundle the
 * server with rollup directly.
 *
 * Deliberately NOT the generated `npm run bundle` script: its `tsc --build`
 * half needs project references and devDeps this deploy does not carry.
 * rollup-plugin-esbuild does its own TS transpilation; the output is
 * behaviorally identical (this is the recipe-verified chain).
 */
export const SERVER_BUILD_COMMAND = [
  "cd .wasp/out",
  "npx prisma generate --schema=db/schema.prisma",
  "DATABASE_URL=$DIRECT_URL npx prisma migrate deploy --schema=db/schema.prisma",
  "cd server && npm install && npx rollup --config --silent",
].join(" && ");

export function makeServerVercelJsonContents(): string {
  return JSON.stringify(
    {
      framework: "express",
      installCommand: "npm install",
      buildCommand: SERVER_BUILD_COMMAND,
    },
    null,
    2,
  );
}

/**
 * The SPA rewrite config placed inside the client build output dir. The
 * fallback filename is resolved by the caller (it differs across Wasp
 * versions - see getClientSpaFallbackFileName), never hardcoded here.
 */
export function makeClientVercelJsonContents(
  spaFallbackFileName: string,
): string {
  return JSON.stringify(
    {
      rewrites: [
        { source: "/(.*)", destination: `/${spaFallbackFileName}` },
      ],
    },
    null,
    2,
  );
}

const VERCEL_BINARY_TARGETS_LINE = `binaryTargets = ["native", "rhel-openssl-3.0.x", "debian-openssl-3.0.x"]`;
const DIRECT_URL_LINE = `directUrl = env("DIRECT_URL")`;

/**
 * Patches the generated Prisma schema for Vercel, unconditionally:
 *   - the generator client block must carry Vercel-compatible engine
 *     binaries, or the deployed function throws "Query Engine not found";
 *   - the datasource block gets `directUrl = env("DIRECT_URL")` so every
 *     Prisma migrate invocation uses the direct (non-pooled) connection.
 */
export function patchPrismaSchemaForVercel(schemaContents: string): string {
  const withBinaryTargets = upsertLineInPrismaBlock(
    schemaContents,
    /generator\s+client\s*\{[^}]*\}/,
    "generator client",
    /binaryTargets\s*=\s*\[[^\]]*\]/,
    VERCEL_BINARY_TARGETS_LINE,
  );
  return upsertLineInPrismaBlock(
    withBinaryTargets,
    /datasource\s+\w+\s*\{[^}]*\}/,
    "datasource",
    /directUrl\s*=\s*[^\n]+/,
    DIRECT_URL_LINE,
  );
}

function upsertLineInPrismaBlock(
  schemaContents: string,
  blockRe: RegExp,
  blockDescription: string,
  existingLineRe: RegExp,
  newLine: string,
): string {
  const blockMatch = schemaContents.match(blockRe);
  if (blockMatch === null) {
    throw new Error(
      `Could not find the ${blockDescription} block in the generated Prisma schema.`,
    );
  }
  const block = blockMatch[0];
  const patchedBlock = existingLineRe.test(block)
    ? block.replace(existingLineRe, newLine)
    : block.replace(/\}$/, `  ${newLine}\n}`);
  // Replacement via callback so "$" sequences in the block are inert.
  return schemaContents.replace(block, () => patchedBlock);
}

// What the generated Dockerfile copies to the image root and to the nested
// .wasp/out/ dir, respectively. The workspaces globs in the generated
// package.json (".wasp/out/*", ".wasp/out/sdk/wasp") are relative to this
// layout, so the file is staged verbatim - no rewrite.
const STAGED_ROOT_DIRS = ["src"];
const STAGED_ROOT_FILES = ["package.json", "package-lock.json", "tsconfig.json"];
const STAGED_BUILD_OUTPUT_DIRS = ["server", "sdk", "libs", "db"];
const REQUIRED_BUILD_OUTPUT_ENTRIES = ["server", "db", "package.json"];

/**
 * Builds the ephemeral server deploy root from the (freshly built) Wasp
 * build output: the Docker-image layout mirrored onto disk, plus the Vercel
 * artifacts (entrypoint shim, vercel.json, patched Prisma schema). Returns
 * the staging dir; the caller deploys it and removes it afterwards. Rebuilt
 * on every deploy - `wasp build` regenerates its inputs each time it runs.
 */
export function createServerDeployStagingDir(
  serverBuildArtefactsDir: string,
): string {
  for (const requiredEntry of REQUIRED_BUILD_OUTPUT_ENTRIES) {
    if (!fs.existsSync(path.join(serverBuildArtefactsDir, requiredEntry))) {
      throw new Error(
        `The Wasp build output at ${serverBuildArtefactsDir} is missing "${requiredEntry}". Did \`wasp build\` succeed?`,
      );
    }
  }

  const stagingDir = fs.mkdtempSync(
    path.join(os.tmpdir(), "wasp-vercel-server-deploy-"),
  );

  for (const dir of STAGED_ROOT_DIRS) {
    copyIfExists(
      path.join(serverBuildArtefactsDir, dir),
      path.join(stagingDir, dir),
    );
  }
  for (const file of STAGED_ROOT_FILES) {
    copyIfExists(
      path.join(serverBuildArtefactsDir, file),
      path.join(stagingDir, file),
    );
  }
  for (const dir of STAGED_BUILD_OUTPUT_DIRS) {
    copyIfExists(
      path.join(serverBuildArtefactsDir, dir),
      path.join(stagingDir, ".wasp", "out", dir),
    );
  }

  const schemaPath = path.join(
    stagingDir,
    ".wasp",
    "out",
    "db",
    "schema.prisma",
  );
  fs.writeFileSync(
    schemaPath,
    patchPrismaSchemaForVercel(fs.readFileSync(schemaPath, "utf-8")),
  );

  fs.writeFileSync(path.join(stagingDir, "server.js"), SERVER_ENTRYPOINT_SHIM);
  fs.writeFileSync(
    path.join(stagingDir, "vercel.json"),
    makeServerVercelJsonContents(),
  );

  return stagingDir;
}

function copyIfExists(source: string, destination: string): void {
  if (!fs.existsSync(source)) {
    return;
  }
  fs.mkdirSync(path.dirname(destination), { recursive: true });
  fs.cpSync(source, destination, {
    recursive: true,
    // node_modules are reinstalled remotely; .vercel would leak a stale
    // project link into the upload.
    filter: (src) => {
      const baseName = path.basename(src);
      return baseName !== "node_modules" && baseName !== ".vercel";
    },
  });
}

// ---------------------------------------------------------------------------
// Orchestration
// ---------------------------------------------------------------------------

export async function deploy(
  appName: string,
  options: VercelDeployCmdOptions,
): Promise<void> {
  const vercelCli = createVercelCli({
    vercelExe: options.vercelExe,
    token: options.token,
    scope: options.scope,
  });
  await runDeploy(vercelCli, appName, options);
}

/**
 * The deploy orchestration, with the Vercel CLI and the build steps
 * injected as boundaries so unit tests can run it without spawning
 * processes:
 *   1. verify both projects exist (setup's job to create them),
 *   2. `wasp build` (memoized per CLI invocation),
 *   3. server: apply the deploy artifacts to `.wasp/out`, link, deploy,
 *   4. client: Vite build with the predicted server URL baked in, write
 *      the SPA-rewrite vercel.json, link, deploy.
 */
export async function runDeploy(
  vercelCli: VercelCli,
  appName: string,
  options: VercelDeployCmdOptions,
  boundary: DeployBoundary = defaultBoundary,
): Promise<void> {
  assertVercelAppNameIsValid(appName);

  waspSays("Deploying your Wasp app to Vercel!");

  const serverProjectName = getServerProjectName(appName);
  const clientProjectName = getClientProjectName(appName);
  // Fail fast, before the (expensive) build: deploy never creates projects
  // or env vars - that is setup's job.
  await assertProjectIsSetUp(vercelCli, serverProjectName, appName);
  await assertProjectIsSetUp(vercelCli, clientProjectName, appName);

  await boundary.ensureWaspProjectIsBuilt(options);

  await deployServer(vercelCli, serverProjectName, appName, options);
  await deployClientApp(vercelCli, clientProjectName, appName, options, boundary);

  displayWaspRocketImage();
  waspSays(
    [
      "Your Wasp app has been deployed to Vercel!",
      `  - Client: ${getClientAppUrl(appName)}`,
      `  - Server: ${getServerAppUrl(appName)}`,
    ].join("\n"),
  );
}

async function assertProjectIsSetUp(
  vercelCli: VercelCli,
  projectName: string,
  appName: string,
): Promise<void> {
  if (await vercelCli.doesProjectExist(projectName)) {
    return;
  }
  throw new Error(
    [
      `Vercel project "${projectName}" was not found.`,
      `Run \`wasp deploy vercel setup ${appName}\` first, or use`,
      `\`wasp deploy vercel launch ${appName}\` to set up and deploy in one go.`,
    ].join("\n"),
  );
}

async function deployServer(
  vercelCli: VercelCli,
  serverProjectName: string,
  appName: string,
  options: VercelDeployCmdOptions,
): Promise<void> {
  waspSays("Deploying your server now...");

  const serverBuildArtefactsDir = getServerBuildArtefactsDir(
    options.waspProjectDir,
  );
  const stagingDir = createServerDeployStagingDir(serverBuildArtefactsDir);
  try {
    // The staging dir is fresh on every deploy, so it always needs a link.
    await vercelCli.linkProjectToDir(serverProjectName, stagingDir);

    waspSays(
      "Creating the server deployment (Vercel builds it remotely - this can take a few minutes)...",
    );
    await vercelCli.deployToProd(stagingDir);
  } finally {
    fs.rmSync(stagingDir, { recursive: true, force: true });
  }

  waspSays(`Server has been deployed at: ${getServerAppUrl(appName)}`);
}

async function deployClientApp(
  vercelCli: VercelCli,
  clientProjectName: string,
  appName: string,
  options: VercelDeployCmdOptions,
  boundary: DeployBoundary,
): Promise<void> {
  waspSays("Deploying your client now...");

  // The server URL is baked into the client bundle at Vite build time (the
  // deterministic prediction makes this possible before/without parsing any
  // deploy output).
  const serverUrl = getServerAppUrl(appName);
  const clientBuildArtefactsDir = await boundary.buildClient(
    serverUrl,
    options,
  );

  writeClientSpaRewriteConfig(options.waspProjectDir, clientBuildArtefactsDir);

  await vercelCli.linkProjectToDir(clientProjectName, clientBuildArtefactsDir);
  await vercelCli.deployToProd(clientBuildArtefactsDir);

  waspSays(`Client has been deployed at: ${getClientAppUrl(appName)}`);
}

function writeClientSpaRewriteConfig(
  waspProjectDir: WaspProjectDir,
  clientBuildArtefactsDir: string,
): void {
  // The build output dir has no package.json, so Vercel deploys it as a
  // plain static site and this vercel.json is the only config it reads.
  // The fallback filename drifts across Wasp versions - always resolve it
  // through the shared helper.
  const spaFallbackFileName = getClientSpaFallbackFileName(waspProjectDir);
  fs.writeFileSync(
    path.join(clientBuildArtefactsDir, "vercel.json"),
    makeClientVercelJsonContents(spaFallbackFileName),
  );
}
