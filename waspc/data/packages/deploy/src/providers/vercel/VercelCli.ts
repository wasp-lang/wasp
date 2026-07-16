import fs from "fs";
import path from "node:path";

import { $ } from "zx";

/**
 * Thin, non-interactive wrappers around the `vercel` CLI (the only process
 * boundary of the setup command - unit tests fake this interface instead of
 * spawning processes).
 *
 * Every command is invoked with `--token`/`--scope` when provided so it
 * works headless (CI, agents) without a `vercel login` session, and with
 * `--cwd` for the commands that operate on a linked project directory.
 * Secret values are always piped via stdin, never passed in argv.
 */
export interface VercelCli {
  doesProjectExist(projectName: string): Promise<boolean>;
  createProject(projectName: string): Promise<void>;
  linkProjectToDir(projectName: string, dir: string): Promise<void>;
  setEnvVar(
    linkedProjectDir: string,
    name: string,
    value: string,
    environment?: string,
  ): Promise<void>;
  pullEnvVars(
    linkedProjectDir: string,
    environment: string,
  ): Promise<Record<string, string>>;
  listEnvVarNames(
    linkedProjectDir: string,
    environment?: string,
  ): Promise<string[]>;
  isIntegrationInstalled(integrationSlug: string): Promise<boolean>;
  addIntegrationResource(args: {
    integrationSlug: string;
    resourceName: string;
    linkedProjectDir: string;
  }): Promise<{ success: boolean; output: string }>;
  connectIntegrationResource(args: {
    resourceName: string;
    projectName: string;
    linkedProjectDir: string;
  }): Promise<{ success: boolean; output: string }>;
  deployToProd(linkedProjectDir: string): Promise<string>;
}

export interface VercelCliOptions {
  vercelExe: string;
  token?: string;
  scope?: string;
}

export function createVercelCli(options: VercelCliOptions): VercelCli {
  return new ZxVercelCli(options);
}

class ZxVercelCli implements VercelCli {
  private readonly vercelExe: string;
  private readonly token?: string;
  private readonly authArgs: string[];

  constructor({ vercelExe, token, scope }: VercelCliOptions) {
    this.vercelExe = vercelExe;
    this.token = token;
    this.authArgs = [
      ...(token ? ["--token", token] : []),
      ...(scope ? ["--scope", scope] : []),
    ];
  }

  async doesProjectExist(projectName: string): Promise<boolean> {
    const result = await this.runVercelCommand(
      ["project", "inspect", projectName],
      { nothrow: true, quiet: true },
    );
    return result.exitCode === 0;
  }

  async createProject(projectName: string): Promise<void> {
    await this.runVercelCommand(["project", "add", projectName]);
  }

  async linkProjectToDir(projectName: string, dir: string): Promise<void> {
    await this.runVercelCommand([
      "link",
      "--yes",
      ...["--project", projectName],
      ...["--cwd", dir],
    ]);
  }

  async setEnvVar(
    linkedProjectDir: string,
    name: string,
    value: string,
    environment: string = "production",
  ): Promise<void> {
    // `--force` overwrites an existing variable for the same target, which
    // keeps re-runs of `setup` idempotent. The value is piped via stdin so
    // it never shows up in argv or logs. `--no-sensitive` keeps the
    // behavior deterministic: when the Vercel CLI detects an agent (or any
    // non-interactive run) it otherwise defaults production env vars to
    // "sensitive", whose values are write-only and would break both
    // troubleshooting and this provider's own DATABASE_URL_UNPOOLED ->
    // DIRECT_URL mapping via `env pull`.
    await this.runVercelCommand(
      [
        "env",
        "add",
        name,
        environment,
        "--force",
        "--no-sensitive",
        ...["--cwd", linkedProjectDir],
      ],
      { input: value, quiet: true },
    );
  }

  async pullEnvVars(
    linkedProjectDir: string,
    environment: string,
  ): Promise<Record<string, string>> {
    // Pull into a throwaway file inside the (temporary) linked dir, parse
    // it, and delete it right away so no secret values linger on disk.
    const envFileName = `.env.wasp-vercel-setup.${environment}`;
    const envFilePath = path.join(linkedProjectDir, envFileName);
    try {
      await this.runVercelCommand(
        [
          "env",
          "pull",
          envFileName,
          ...["--environment", environment],
          "--yes",
          ...["--cwd", linkedProjectDir],
        ],
        { quiet: true },
      );
      return parseEnvFile(fs.readFileSync(envFilePath, "utf-8"));
    } finally {
      fs.rmSync(envFilePath, { force: true });
    }
  }

  async listEnvVarNames(
    linkedProjectDir: string,
    environment: string = "production",
  ): Promise<string[]> {
    // Lists the env var *names* on the linked project without exposing any
    // value: `env ls --format json` returns each variable's key with the
    // value hidden (unlike `env pull`, which decrypts and writes secrets to
    // disk). Used by setup's JWT_SECRET skip-if-exists guard so a re-run does
    // not regenerate a secret that already exists.
    const result = await this.runVercelCommand(
      [
        "env",
        "ls",
        environment,
        ...["--format", "json"],
        ...["--cwd", linkedProjectDir],
      ],
      { quiet: true },
    );
    return parseEnvLsNames(result.stdout);
  }

  async isIntegrationInstalled(integrationSlug: string): Promise<boolean> {
    const result = await this.runVercelCommand(
      [
        "integration",
        "installations",
        ...["--integration", integrationSlug],
        "--format=json",
      ],
      { nothrow: true, quiet: true },
    );
    if (result.exitCode === 0 && jsonOutputListsAnInstallation(result.stdout)) {
      return true;
    }
    // Vercel CLI 56.x scopes `integration installations` to the personal
    // account, so team-level Marketplace installations come back as an
    // empty list (observed live: `integration add neon` succeeded right
    // after `installations` returned an empty list). Fall back to probing
    // the team installation via `integration balance`, which resolves the
    // installation first and fails with a distinctive message when the
    // integration is not installed.
    const probe = await this.runVercelCommand(
      ["integration", "balance", integrationSlug],
      { nothrow: true, quiet: true },
    );
    return balanceProbeShowsAnInstallation({
      exitCode: probe.exitCode ?? 1,
      output: `${probe.stdout}\n${probe.stderr}`,
    });
  }

  async addIntegrationResource({
    integrationSlug,
    resourceName,
    linkedProjectDir,
  }: {
    integrationSlug: string;
    resourceName: string;
    linkedProjectDir: string;
  }): Promise<{ success: boolean; output: string }> {
    // Connects the new resource to the project linked in `linkedProjectDir`.
    // `--no-claim` prevents blocking on the sandbox-claim prompt and
    // `--no-env-pull` keeps the CLI from writing an .env file as a side
    // effect (we pull env vars explicitly when we need them).
    const result = await this.runVercelCommand(
      [
        "integration",
        "add",
        integrationSlug,
        ...["--name", resourceName],
        ...["-e", "production", "-e", "preview", "-e", "development"],
        "--no-claim",
        "--no-env-pull",
        ...["--cwd", linkedProjectDir],
      ],
      { nothrow: true },
    );
    if (result.exitCode === 0) {
      await this.waitForDatabaseUrlInjection(linkedProjectDir);
    }
    return {
      success: result.exitCode === 0,
      output: `${result.stdout}\n${result.stderr}`.trim(),
    };
  }

  async connectIntegrationResource({
    resourceName,
    projectName,
    linkedProjectDir,
  }: {
    resourceName: string;
    projectName: string;
    linkedProjectDir: string;
  }): Promise<{ success: boolean; output: string }> {
    const result = await this.runVercelCommand(
      [
        "integration",
        "resource",
        "connect",
        resourceName,
        projectName,
        ...["-e", "production", "-e", "preview", "-e", "development"],
        "--yes",
        ...["--cwd", linkedProjectDir],
      ],
      { nothrow: true },
    );
    if (result.exitCode === 0) {
      await this.waitForDatabaseUrlInjection(linkedProjectDir);
      return {
        success: true,
        output: `${result.stdout}\n${result.stderr}`.trim(),
      };
    }
    // `resource connect` is not idempotent: re-connecting an
    // already-connected resource fails on its own injected env vars
    // ("Cannot connect: env var ... already exists on project ...").
    // Already-connected must count as connected, so a failed connect is a
    // success exactly when the project already has a DATABASE_URL.
    const envVarsOnProject = await this.pullEnvVars(
      linkedProjectDir,
      "production",
    ).catch(() => ({}) as Record<string, string>);
    return resolveFailedConnectOutcome(
      `${result.stdout}\n${result.stderr}`.trim(),
      envVarsOnProject,
    );
  }

  /**
   * The Marketplace injects a database resource's env vars asynchronously
   * (observed live: `integration add neon` returned ~40s before
   * DATABASE_URL appeared on the project). Poll until the injection lands
   * so callers can read the connection strings right after add/connect.
   * On timeout this returns normally - the caller's own DATABASE_URL check
   * reports the real error.
   */
  private async waitForDatabaseUrlInjection(
    linkedProjectDir: string,
  ): Promise<void> {
    await waitForInjectedDatabaseUrl(() =>
      this.pullEnvVars(linkedProjectDir, "production"),
    );
  }

  async deployToProd(linkedProjectDir: string): Promise<string> {
    // Creates a new production deployment for the project linked to
    // `linkedProjectDir`. Non-interactive (`--yes`); the deployment URL is
    // what the CLI prints to stdout. Like every other command here it runs
    // quiet so the token never leaks through zx's verbose argv echo.
    //
    // Uploads flake with transient network errors ("fetch failed" observed
    // live, mid-upload). The CLI's own error JSON advises "retry deploy",
    // and retries converge: already-uploaded files are deduplicated
    // server-side, so every attempt uploads strictly less. Retry a bounded
    // number of times, but only for transient network failures - remote
    // build failures surface immediately.
    const maxAttempts = 3;
    for (let attempt = 1; ; attempt++) {
      const result = await this.runVercelCommand(
        ["deploy", "--prod", "--yes", ...["--cwd", linkedProjectDir]],
        { nothrow: true },
      );
      if (result.exitCode === 0) {
        return result.stdout.trim();
      }
      const output = `${result.stdout}\n${result.stderr}`.trim();
      if (attempt >= maxAttempts || !isTransientDeployFailure(output)) {
        throw new Error(
          [
            `Vercel CLI command failed: ${this.vercelExe} deploy --prod --yes --cwd ${linkedProjectDir}`,
            this.maskToken(output),
          ].join("\n"),
        );
      }
      await this.sleep(5_000);
    }
  }

  // Injection point so a future test can fake time; production sleeps.
  private sleep(ms: number): Promise<void> {
    return new Promise((resolve) => setTimeout(resolve, ms));
  }

  private async runVercelCommand(
    commandArgs: string[],
    options: { nothrow?: boolean; quiet?: boolean; input?: string } = {},
  ) {
    // Always run quiet: this package sets zx's global `$.verbose = true`,
    // which would otherwise echo the full argv - including the `--token`
    // value - into the terminal/logs. Failures are rethrown below with the
    // token masked for the same reason.
    const result = await $({
      nothrow: true,
      quiet: true,
      ...(options.input !== undefined ? { input: options.input } : {}),
    })`${this.vercelExe} ${[...commandArgs, ...this.authArgs]}`;
    if (result.exitCode !== 0 && !(options.nothrow ?? false)) {
      throw new Error(
        [
          `Vercel CLI command failed: ${this.vercelExe} ${this.maskToken(commandArgs.join(" "))}`,
          this.maskToken(`${result.stdout}\n${result.stderr}`.trim()),
        ].join("\n"),
      );
    }
    return result;
  }

  private maskToken(text: string): string {
    // (`split().join()` instead of `replaceAll` - tsconfig targets es2020.)
    return this.token ? text.split(this.token).join("<token>") : text;
  }
}

/**
 * Parses the dotenv-style file `vercel env pull` writes
 * (`KEY="value"` lines, plus comments and blank lines).
 */
export function parseEnvFile(contents: string): Record<string, string> {
  const envVars: Record<string, string> = {};
  for (const line of contents.split("\n")) {
    const match = line.match(/^([A-Za-z_][A-Za-z0-9_]*)=(.*)$/);
    if (match === null) {
      continue;
    }
    const [, name, rawValue] = match;
    envVars[name] = stripSurroundingQuotes(rawValue.trim());
  }
  return envVars;
}

/**
 * Extracts env var names from the JSON `vercel env ls --format json` prints.
 * That command emits `{ "envs": [ { "key": "JWT_SECRET", ... }, ... ] }`
 * (values omitted for encrypted/sensitive vars). Defensive about the shape:
 * accepts a bare array too, and falls back to `name` when `key` is absent.
 * Returns [] for unparseable output so callers can decide what a missing
 * listing means.
 */
export function parseEnvLsNames(stdout: string): string[] {
  let parsed: unknown;
  try {
    parsed = JSON.parse(stdout);
  } catch {
    return [];
  }
  const entries = Array.isArray(parsed)
    ? parsed
    : isObjectWithArrayField(parsed, "envs")
      ? parsed.envs
      : [];
  const names: string[] = [];
  for (const entry of entries) {
    if (typeof entry === "object" && entry !== null) {
      const record = entry as Record<string, unknown>;
      const name = record.key ?? record.name;
      if (typeof name === "string" && name.length > 0) {
        names.push(name);
      }
    }
  }
  return names;
}

function stripSurroundingQuotes(value: string): string {
  if (value.length >= 2 && value.startsWith('"') && value.endsWith('"')) {
    return value.slice(1, -1);
  }
  return value;
}

/**
 * Polls the given env-var pull until it reports a DATABASE_URL, sleeping
 * between attempts, and returns the last pulled record. Never throws on
 * timeout: after the attempt budget is spent, the (DATABASE_URL-less)
 * record is returned and the caller reports the real error. The attempt
 * budget (18 x 5s) comfortably covers the ~40s injection delay observed
 * live without hanging setup forever when provisioning silently failed.
 */
export async function waitForInjectedDatabaseUrl(
  pullEnvVars: () => Promise<Record<string, string>>,
  options: {
    maxAttempts?: number;
    delayMs?: number;
    sleep?: (ms: number) => Promise<void>;
  } = {},
): Promise<Record<string, string>> {
  const maxAttempts = options.maxAttempts ?? 18;
  const delayMs = options.delayMs ?? 5_000;
  const sleep =
    options.sleep ??
    ((ms: number) => new Promise<void>((resolve) => setTimeout(resolve, ms)));

  let envVars = await pullEnvVars();
  for (
    let attempt = 1;
    attempt < maxAttempts && !envVars["DATABASE_URL"];
    attempt++
  ) {
    await sleep(delayMs);
    envVars = await pullEnvVars();
  }
  return envVars;
}

/**
 * Classifies a failed `vercel deploy` by its output: retryable only for
 * transient network errors (the CLI's "fetch failed" upload error and
 * common socket-level failures). Anything else - remote build failures,
 * usage errors - must surface immediately instead of burning retries.
 */
export function isTransientDeployFailure(output: string): boolean {
  return /fetch failed|ECONNRESET|ETIMEDOUT|ECONNREFUSED|EAI_AGAIN|EPIPE|socket hang up|network error/i.test(
    output,
  );
}

/**
 * Decides what a failed `integration resource connect` means: success when
 * the linked project already carries a DATABASE_URL (the resource - or an
 * equivalent database - is already wired up), otherwise the original
 * failure with its output preserved for the caller's error message.
 */
export function resolveFailedConnectOutcome(
  output: string,
  envVarsOnProject: Record<string, string>,
): { success: boolean; output: string } {
  if (envVarsOnProject["DATABASE_URL"]) {
    return { success: true, output };
  }
  return { success: false, output };
}

/**
 * Interprets the output of the `integration balance <slug>` fallback probe:
 * the integration is not installed exactly when the CLI failed while saying
 * so ("No installation(s) found ..."). Any other failure (e.g. an installed
 * integration without balance info, or a transient network error) must not
 * report "not installed" - setup's later steps surface the real error.
 */
export function balanceProbeShowsAnInstallation(probe: {
  exitCode: number;
  output: string;
}): boolean {
  if (probe.exitCode === 0) {
    return true;
  }
  return !/No installations? found/i.test(probe.output);
}

function jsonOutputListsAnInstallation(stdout: string): boolean {
  try {
    const parsed: unknown = JSON.parse(stdout);
    const installations = Array.isArray(parsed)
      ? parsed
      : isObjectWithArrayField(parsed, "installations")
        ? parsed.installations
        : null;
    if (installations !== null) {
      return installations.length > 0;
    }
  } catch {
    // Not JSON - fall through to the textual heuristic below.
  }
  // Installation configuration IDs look like `icfg_...` - seeing one means
  // at least one installation exists even if the JSON shape changed.
  return stdout.includes("icfg_");
}

function isObjectWithArrayField<Field extends string>(
  value: unknown,
  field: Field,
): value is { [key in Field]: unknown[] } {
  return (
    typeof value === "object" &&
    value !== null &&
    Array.isArray((value as Record<string, unknown>)[field])
  );
}
