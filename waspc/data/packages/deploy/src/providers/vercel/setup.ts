import fs from "fs";
import os from "node:os";
import path from "node:path";

import { WaspCliExe, WaspProjectDir } from "../../common/brandedTypes.js";
import { generateRandomHexString } from "../../common/random.js";
import { waspSays } from "../../common/terminal.js";
import {
  assertVercelAppNameIsValid,
  getClientAppUrl,
  getClientProjectName,
  getServerAppUrl,
  getServerProjectName,
} from "./brandedUrls.js";
import { findWaspEntryFile } from "./preflight.js";
import { createVercelCli, VercelCli } from "./VercelCli.js";

// Re-exported for backwards compatibility: these helpers used to live here
// before task-15 moved them into brandedUrls.ts.
export {
  assertVercelAppNameIsValid,
  getClientAppUrl,
  getClientProjectName,
  getServerAppUrl,
  getServerProjectName,
};

export interface VercelSetupCmdOptions {
  waspExe: WaspCliExe;
  waspProjectDir: WaspProjectDir;
  vercelExe: string;
  token?: string;
  scope?: string;
  db?: string;
  databaseUrl?: string;
  directUrl?: string;
  serverSecret: string[];
}

const NEON_INTEGRATION_SLUG = "neon";
const PRODUCTION_ENVIRONMENT = "production";

export interface ServerSecret {
  name: string;
  value: string;
}

export function parseServerSecrets(secrets: string[]): ServerSecret[] {
  return secrets.map((secret) => {
    const separatorIndex = secret.indexOf("=");
    if (separatorIndex === -1) {
      throw new Error(
        `Invalid --server-secret "${secret}": expected NAME=VALUE.`,
      );
    }
    const name = secret.slice(0, separatorIndex);
    const value = secret.slice(separatorIndex + 1);
    if (!/^[A-Za-z_][A-Za-z0-9_]*$/.test(name)) {
      throw new Error(
        `Invalid --server-secret name "${name}": env var names must start ` +
          "with a letter or underscore and contain only letters, digits and underscores.",
      );
    }
    return { name, value };
  });
}

// Matches the auth declaration in both syntaxes: the legacy `main.wasp`
// DSL (`auth: { ... }` inside the app block) and the TS spec
// (`auth({ ... })` in `main.wasp.ts`). This is the same kind of textual
// heuristic the preflight checks use - not a full Wasp parser.
const AUTH_RE = /\bauth\s*(?:\(|[:=]\s*\{)/;

export function appUsesAuth(waspProjectDir: WaspProjectDir): boolean {
  const entryFile = findWaspEntryFile(waspProjectDir);
  if (entryFile === undefined) {
    return false;
  }
  try {
    return AUTH_RE.test(fs.readFileSync(entryFile, "utf-8"));
  } catch {
    return false;
  }
}

export async function setup(
  appName: string,
  options: VercelSetupCmdOptions,
): Promise<void> {
  const vercelCli = createVercelCli({
    vercelExe: options.vercelExe,
    token: options.token,
    scope: options.scope,
  });
  await runSetup(vercelCli, appName, options);
}

/**
 * The setup orchestration, with the Vercel CLI injected as a boundary so
 * unit tests can run it against an in-memory fake:
 *   1. create the `<app>-server` and `<app>-client` projects (skip-if-exists),
 *   2. wire a database (Neon via the Vercel Marketplace by default, or the
 *      `--database-url` escape hatch),
 *   3. set the server env vars (DATABASE_URL, DIRECT_URL, WASP_SERVER_URL,
 *      WASP_WEB_CLIENT_URL, JWT_SECRET for auth apps, --server-secret
 *      passthroughs).
 */
export async function runSetup(
  vercelCli: VercelCli,
  appName: string,
  options: VercelSetupCmdOptions,
): Promise<void> {
  waspSays("Setting up your Wasp app with Vercel!");

  // Validate everything up front so we fail before mutating anything.
  assertVercelAppNameIsValid(appName);
  const serverSecrets = parseServerSecrets(options.serverSecret);
  const dbPlan = determineDbPlan(options);
  if (dbPlan.kind === "neon") {
    await assertNeonIntegrationIsInstalled(vercelCli);
  }

  const serverProjectName = getServerProjectName(appName);
  const clientProjectName = getClientProjectName(appName);

  await ensureProjectExists(vercelCli, serverProjectName);
  await ensureProjectExists(vercelCli, clientProjectName);

  // Vercel's project-scoped commands (env, integration) operate on the
  // project linked to the current directory. `.wasp/out` is wiped on every
  // `wasp build`, so instead of linking inside the project we link the
  // server project to a throwaway temp dir for the duration of setup.
  await withTempDir(async (serverLinkDir) => {
    await vercelCli.linkProjectToDir(serverProjectName, serverLinkDir);

    await setupDatabase({
      vercelCli,
      appName,
      serverProjectName,
      serverLinkDir,
      dbPlan,
    });

    waspSays(`Setting up server env vars on project "${serverProjectName}"`);
    await vercelCli.setEnvVar(
      serverLinkDir,
      "WASP_SERVER_URL",
      getServerAppUrl(appName),
    );
    await vercelCli.setEnvVar(
      serverLinkDir,
      "WASP_WEB_CLIENT_URL",
      getClientAppUrl(appName),
    );

    if (appUsesAuth(options.waspProjectDir)) {
      // Skip-if-exists (mirrors the Railway provider): JWT_SECRET must be
      // generated only once. Regenerating it on every setup re-run would
      // rotate the signing key and invalidate every previously issued
      // session/JWT. WASP_SERVER_URL and WASP_WEB_CLIENT_URL above are
      // deterministic, so re-setting those is harmless; a random secret is
      // not.
      const existingServerEnvVars = await vercelCli.listEnvVarNames(
        serverLinkDir,
        PRODUCTION_ENVIRONMENT,
      );
      if (existingServerEnvVars.includes("JWT_SECRET")) {
        waspSays("JWT_SECRET already set. Skipping.");
      } else {
        waspSays("Auth detected: generating a random JWT_SECRET.");
        await vercelCli.setEnvVar(
          serverLinkDir,
          "JWT_SECRET",
          generateRandomHexString(),
        );
      }
    }

    for (const secret of serverSecrets) {
      await vercelCli.setEnvVar(serverLinkDir, secret.name, secret.value);
    }
  });

  waspSays(
    [
      "Vercel setup complete!",
      `  - Server project: ${serverProjectName} (${getServerAppUrl(appName)})`,
      `  - Client project: ${clientProjectName} (${getClientAppUrl(appName)})`,
      `Deploy the app with: wasp deploy vercel deploy ${appName}`,
    ].join("\n"),
  );
}

type DbPlan =
  | { kind: "neon" }
  | { kind: "custom-url"; databaseUrl: string; directUrl: string };

function determineDbPlan(options: VercelSetupCmdOptions): DbPlan {
  if (options.databaseUrl !== undefined) {
    return {
      kind: "custom-url",
      databaseUrl: options.databaseUrl,
      // Without a separate non-pooled URL, migrations use the same one.
      directUrl: options.directUrl ?? options.databaseUrl,
    };
  }
  const db = options.db ?? "neon";
  if (db !== "neon") {
    throw new Error(
      `Unsupported --db value: "${db}". Only "neon" is supported ` +
        "(or pass --database-url to bring your own database).",
    );
  }
  return { kind: "neon" };
}

async function ensureProjectExists(
  vercelCli: VercelCli,
  projectName: string,
): Promise<void> {
  if (await vercelCli.doesProjectExist(projectName)) {
    waspSays(`Project "${projectName}" already exists. Skipping creation.`);
    return;
  }
  waspSays(`Creating Vercel project "${projectName}"`);
  await vercelCli.createProject(projectName);
}

async function assertNeonIntegrationIsInstalled(
  vercelCli: VercelCli,
): Promise<void> {
  if (await vercelCli.isIntegrationInstalled(NEON_INTEGRATION_SLUG)) {
    return;
  }
  throw new Error(
    [
      "The Neon integration is not installed on your Vercel team yet.",
      "Installing it is a one-time step that needs a human. Either:",
      "  - run `vercel integration accept-terms neon` in an interactive terminal, or",
      "  - install it via the Vercel dashboard: your team -> Storage -> Browse Marketplace -> Neon -> Install.",
      "Then re-run this command - it resumes automatically once the integration is installed.",
      "Alternatively, pass --database-url <url> to use a database you already have.",
    ].join("\n"),
  );
}

async function setupDatabase({
  vercelCli,
  appName,
  serverProjectName,
  serverLinkDir,
  dbPlan,
}: {
  vercelCli: VercelCli;
  appName: string;
  serverProjectName: string;
  serverLinkDir: string;
  dbPlan: DbPlan;
}): Promise<void> {
  if (dbPlan.kind === "custom-url") {
    waspSays("Using the provided --database-url for DATABASE_URL.");
    await vercelCli.setEnvVar(
      serverLinkDir,
      "DATABASE_URL",
      dbPlan.databaseUrl,
    );
    await vercelCli.setEnvVar(serverLinkDir, "DIRECT_URL", dbPlan.directUrl);
    return;
  }

  const resourceName = `${appName}-db`;
  waspSays(
    `Provisioning Neon Postgres "${resourceName}" via the Vercel Marketplace`,
  );
  const addResult = await vercelCli.addIntegrationResource({
    integrationSlug: NEON_INTEGRATION_SLUG,
    resourceName,
    linkedProjectDir: serverLinkDir,
  });
  if (!addResult.success) {
    // Most common cause: the resource already exists from a previous setup
    // run. Connecting it to the server project keeps setup idempotent.
    waspSays(
      `Provisioning failed - trying to connect an existing "${resourceName}" resource instead.`,
    );
    const connectResult = await vercelCli.connectIntegrationResource({
      resourceName,
      projectName: serverProjectName,
      linkedProjectDir: serverLinkDir,
    });
    if (!connectResult.success) {
      throw new Error(
        [
          `Could not provision or connect the Neon database "${resourceName}".`,
          `Provisioning output:\n${indent(addResult.output)}`,
          `Connecting output:\n${indent(connectResult.output)}`,
          "You can provision a database yourself (Vercel dashboard -> Storage -> Neon,",
          "or any Postgres provider) and re-run this command with --database-url <url>.",
        ].join("\n"),
      );
    }
  }

  // The integration injects DATABASE_URL (pooled) and DATABASE_URL_UNPOOLED
  // (direct) into the project. Prisma migrations must not run through the
  // pooler, so map the direct URL to DIRECT_URL (the name our Prisma
  // schema patch and vercel.json buildCommand expect).
  const injectedEnvVars = await vercelCli.pullEnvVars(
    serverLinkDir,
    PRODUCTION_ENVIRONMENT,
  );
  const databaseUrl = injectedEnvVars["DATABASE_URL"];
  if (databaseUrl === undefined || databaseUrl === "") {
    throw new Error(
      [
        `The Neon integration did not inject a DATABASE_URL into project "${serverProjectName}".`,
        "Check the resource in the Vercel dashboard (Storage tab), or re-run this",
        "command with --database-url <url> to set the connection string directly.",
      ].join("\n"),
    );
  }
  const directUrl = injectedEnvVars["DATABASE_URL_UNPOOLED"];
  if (directUrl === undefined || directUrl === "") {
    waspSays(
      "WARN: no DATABASE_URL_UNPOOLED found; using the pooled DATABASE_URL as DIRECT_URL.",
    );
  }
  await vercelCli.setEnvVar(
    serverLinkDir,
    "DIRECT_URL",
    directUrl || databaseUrl,
  );
  waspSays("Neon database connected: DATABASE_URL and DIRECT_URL are set.");
}

async function withTempDir(
  action: (tempDir: string) => Promise<void>,
): Promise<void> {
  const tempDir = fs.mkdtempSync(path.join(os.tmpdir(), "wasp-vercel-setup-"));
  try {
    await action(tempDir);
  } finally {
    fs.rmSync(tempDir, { recursive: true, force: true });
  }
}

function indent(text: string): string {
  return text
    .split("\n")
    .map((line) => `    ${line}`)
    .join("\n");
}
