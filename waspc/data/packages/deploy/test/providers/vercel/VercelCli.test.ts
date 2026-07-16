import { describe, expect, test } from "vitest";

import {
  balanceProbeShowsAnInstallation,
  parseEnvLsNames,
  waitForInjectedDatabaseUrl,
  isTransientDeployFailure,
  resolveFailedConnectOutcome,
} from "../../../src/providers/vercel/VercelCli.js";

// The JWT_SECRET skip-if-exists guard consults `vercel env ls --format json`,
// which prints `{ "envs": [ { "key": ..., ... } ] }` with values hidden for
// encrypted vars. The parser extracts just the names so the guard can tell
// whether a secret is already set without ever pulling its value.
describe("parseEnvLsNames", () => {
  test("extracts keys from the { envs: [...] } shape the CLI emits", () => {
    const stdout = JSON.stringify({
      envs: [
        { key: "JWT_SECRET", value: undefined, type: "encrypted" },
        { key: "DATABASE_URL", value: "postgres://x", type: "plain" },
      ],
    });
    expect(parseEnvLsNames(stdout)).toEqual(["JWT_SECRET", "DATABASE_URL"]);
  });

  test("accepts a bare array and falls back to a `name` field", () => {
    const stdout = JSON.stringify([{ name: "FOO" }, { key: "BAR" }]);
    expect(parseEnvLsNames(stdout)).toEqual(["FOO", "BAR"]);
  });

  test("returns [] for empty listings and unparseable output", () => {
    expect(parseEnvLsNames(JSON.stringify({ envs: [] }))).toEqual([]);
    expect(parseEnvLsNames("Vercel CLI 56.2.1\nnot json")).toEqual([]);
  });
});

// Vercel CLI 56.x scopes `integration installations` to the personal
// account, so team-level Marketplace installations come back as an empty
// list (a false negative observed live on the wpp-ai-coe team, where
// `integration add neon` succeeded right after `installations` returned
// `{"installations": []}`). The fallback probe is `integration balance
// <slug>`, which resolves the team installation first and fails with a
// distinctive message when - and only when - the integration is not
// installed.
describe("balanceProbeShowsAnInstallation", () => {
  test("a successful balance probe means the integration is installed", () => {
    expect(
      balanceProbeShowsAnInstallation({
        exitCode: 0,
        output: "Retrieving resources…\nBalance: $0.00",
      }),
    ).toBe(true);
  });

  test("'No installations found' means the integration is not installed", () => {
    expect(
      balanceProbeShowsAnInstallation({
        exitCode: 1,
        output:
          "Retrieving installation…\nError: No installations found for this integration",
      }),
    ).toBe(false);
  });

  test("'No installation found' (singular) also means not installed", () => {
    expect(
      balanceProbeShowsAnInstallation({
        exitCode: 1,
        output: "Error: No installation found for this integration",
      }),
    ).toBe(false);
  });

  test("an installed integration without balance info still counts as installed", () => {
    expect(
      balanceProbeShowsAnInstallation({
        exitCode: 1,
        output:
          "Retrieving resources…\nRetrieving balance info…\nError: No balance information found for this integration",
      }),
    ).toBe(true);
  });

  test("unrelated failures do not report 'not installed' (setup's later steps surface the real error)", () => {
    expect(
      balanceProbeShowsAnInstallation({
        exitCode: 1,
        output: "Error: Network error",
      }),
    ).toBe(true);
  });
});

// The Marketplace injects a database resource's env vars asynchronously:
// observed live, `integration add neon` returned ~40 seconds before
// DATABASE_URL appeared on the project, so a setup that pulls right away
// races the injection and fails on a fresh app. The boundary must poll
// until DATABASE_URL is visible (or a bounded timeout elapses).
describe("waitForInjectedDatabaseUrl", () => {
  test("returns as soon as a pull sees DATABASE_URL", async () => {
    const pulls = [
      {},
      {},
      { DATABASE_URL: "postgres://pooled" },
    ];
    let pullCount = 0;
    const result = await waitForInjectedDatabaseUrl(
      async () => pulls[pullCount++] ?? {},
      { sleep: async () => {} },
    );
    expect(result).toEqual({ DATABASE_URL: "postgres://pooled" });
    expect(pullCount).toBe(3);
  });

  test("does not sleep or pull again when the first pull already has it", async () => {
    let sleeps = 0;
    let pullCount = 0;
    const result = await waitForInjectedDatabaseUrl(
      async () => {
        pullCount++;
        return { DATABASE_URL: "postgres://pooled", OTHER: "x" };
      },
      {
        sleep: async () => {
          sleeps++;
        },
      },
    );
    expect(result).toEqual({ DATABASE_URL: "postgres://pooled", OTHER: "x" });
    expect(pullCount).toBe(1);
    expect(sleeps).toBe(0);
  });

  test("gives up after the attempt budget and returns the last pull (the caller reports the real error)", async () => {
    let pullCount = 0;
    const result = await waitForInjectedDatabaseUrl(
      async () => {
        pullCount++;
        return { UNRELATED: "still here" };
      },
      { maxAttempts: 4, sleep: async () => {} },
    );
    expect(result).toEqual({ UNRELATED: "still here" });
    expect(pullCount).toBe(4);
  });
});

// `integration resource connect` is not idempotent: when the resource is
// already connected to the project it fails with "Cannot connect: env var
// NEON_PROJECT_ID already exists on project ..." (observed live on a setup
// re-run). Already-connected must count as connected: a failed connect is
// a success exactly when the project already has a DATABASE_URL.
describe("resolveFailedConnectOutcome", () => {
  test("a failed connect on a project that already has DATABASE_URL is success", () => {
    const outcome = resolveFailedConnectOutcome(
      "Error: Cannot connect: env var NEON_PROJECT_ID already exists on project wv15a-server",
      { DATABASE_URL: "postgres://pooled", NEON_PROJECT_ID: "np_1" },
    );
    expect(outcome.success).toBe(true);
  });

  test("a failed connect without DATABASE_URL stays a failure with the original output", () => {
    const outcome = resolveFailedConnectOutcome(
      "Error: resource not found",
      { UNRELATED: "x" },
    );
    expect(outcome).toEqual({
      success: false,
      output: "Error: resource not found",
    });
  });
});

// Deploy uploads flake with transient network errors ("fetch failed" was
// observed live three times, killing otherwise-healthy deploys mid-upload).
// The Vercel CLI's own error JSON advises `"when": "retry deploy"`, and
// retries converge because already-uploaded files are deduplicated
// server-side. Only transient network failures may be retried - build
// failures must surface immediately.
describe("isTransientDeployFailure", () => {
  test("'fetch failed' (the CLI's transient upload error) is retryable", () => {
    expect(
      isTransientDeployFailure(
        '{\n  "status": "error",\n  "reason": "deploy_failed",\n  "message": "fetch failed",\n  "next": [{ "command": "vercel deploy", "when": "retry deploy" }]\n}\nError: fetch failed',
      ),
    ).toBe(true);
  });

  test("common socket-level errors are retryable", () => {
    expect(isTransientDeployFailure("Error: ECONNRESET")).toBe(true);
    expect(isTransientDeployFailure("Error: ETIMEDOUT")).toBe(true);
    expect(isTransientDeployFailure("Error: socket hang up")).toBe(true);
  });

  test("a remote build failure is not retryable", () => {
    expect(
      isTransientDeployFailure(
        "Error: Command \"npm install\" exited with 1\nBuild Failed",
      ),
    ).toBe(false);
  });

  test("a plain CLI usage error is not retryable", () => {
    expect(
      isTransientDeployFailure("Error: unknown or unexpected option: --frob"),
    ).toBe(false);
  });
});
