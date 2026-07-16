import path from "node:path";
import { fileURLToPath } from "node:url";

import { beforeEach, describe, expect, test, vi } from "vitest";

import {
  WaspCliExe,
  WaspProjectDir,
} from "../../../src/common/brandedTypes.js";
import {
  parseEnvFile,
  VercelCli,
} from "../../../src/providers/vercel/VercelCli.js";
import {
  appUsesAuth,
  assertVercelAppNameIsValid,
  getClientAppUrl,
  getClientProjectName,
  getServerAppUrl,
  getServerProjectName,
  parseServerSecrets,
  runSetup,
  VercelSetupCmdOptions,
} from "../../../src/providers/vercel/setup.js";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const fixturesDir = path.join(__dirname, "fixtures");

const appWithAuthDir = path.join(fixturesDir, "appWithAuth") as WaspProjectDir;
const appWithAuthTsDir = path.join(
  fixturesDir,
  "appWithAuthTs",
) as WaspProjectDir;
const appWithNeitherDir = path.join(
  fixturesDir,
  "appWithNeither",
) as WaspProjectDir;

interface EnvVarRecord {
  value: string;
  environment: string;
}

/**
 * In-memory fake of the VercelCli boundary (mirrors how the Railway tests
 * exercise logic against fixture data instead of a live CLI). Records every
 * mutating call so tests can assert on exactly what setup would run.
 */
class FakeVercelCli implements VercelCli {
  existingProjects = new Set<string>();
  createdProjects: string[] = [];
  linkedDirs = new Map<string, string>(); // dir -> project name
  envVars = new Map<string, Map<string, EnvVarRecord>>(); // project -> vars

  neonInstalled = true;
  addIntegrationResourceCalls: {
    integrationSlug: string;
    resourceName: string;
    linkedProjectDir: string;
  }[] = [];
  connectIntegrationResourceCalls: {
    resourceName: string;
    projectName: string;
    linkedProjectDir: string;
  }[] = [];
  addIntegrationResourceResult = { success: true, output: "" };
  connectIntegrationResourceResult = { success: true, output: "" };
  integrationInjectedEnvVars: Record<string, string> = {};
  pullEnvVarsCalls: { linkedProjectDir: string; environment: string }[] = [];

  async doesProjectExist(projectName: string): Promise<boolean> {
    return this.existingProjects.has(projectName);
  }

  async createProject(projectName: string): Promise<void> {
    this.createdProjects.push(projectName);
    this.existingProjects.add(projectName);
  }

  async linkProjectToDir(projectName: string, dir: string): Promise<void> {
    if (!this.existingProjects.has(projectName)) {
      throw new Error(`Cannot link nonexistent project: ${projectName}`);
    }
    this.linkedDirs.set(dir, projectName);
  }

  async setEnvVar(
    linkedProjectDir: string,
    name: string,
    value: string,
    environment: string = "production",
  ): Promise<void> {
    const projectName = this.getLinkedProject(linkedProjectDir);
    if (!this.envVars.has(projectName)) {
      this.envVars.set(projectName, new Map());
    }
    this.envVars.get(projectName)!.set(name, { value, environment });
  }

  async pullEnvVars(
    linkedProjectDir: string,
    environment: string,
  ): Promise<Record<string, string>> {
    this.getLinkedProject(linkedProjectDir);
    this.pullEnvVarsCalls.push({ linkedProjectDir, environment });
    return { ...this.integrationInjectedEnvVars };
  }

  async listEnvVarNames(
    linkedProjectDir: string,
    _environment: string = "production",
  ): Promise<string[]> {
    return this.getEnvVarNames(this.getLinkedProject(linkedProjectDir));
  }

  async isIntegrationInstalled(_integrationSlug: string): Promise<boolean> {
    return this.neonInstalled;
  }

  async addIntegrationResource(args: {
    integrationSlug: string;
    resourceName: string;
    linkedProjectDir: string;
  }): Promise<{ success: boolean; output: string }> {
    this.getLinkedProject(args.linkedProjectDir);
    this.addIntegrationResourceCalls.push(args);
    return this.addIntegrationResourceResult;
  }

  async connectIntegrationResource(args: {
    resourceName: string;
    projectName: string;
    linkedProjectDir: string;
  }): Promise<{ success: boolean; output: string }> {
    this.connectIntegrationResourceCalls.push(args);
    return this.connectIntegrationResourceResult;
  }

  async deployToProd(_linkedProjectDir: string): Promise<string> {
    throw new Error("setup must never deploy (deploy's job)");
  }

  getLinkedProject(dir: string): string {
    const projectName = this.linkedDirs.get(dir);
    if (projectName === undefined) {
      throw new Error(`Directory is not linked to any project: ${dir}`);
    }
    return projectName;
  }

  getEnvVar(projectName: string, name: string): EnvVarRecord | undefined {
    return this.envVars.get(projectName)?.get(name);
  }

  getEnvVarNames(projectName: string): string[] {
    return Array.from(this.envVars.get(projectName)?.keys() ?? []);
  }
}

function makeOptions(
  overrides: Partial<VercelSetupCmdOptions> = {},
): VercelSetupCmdOptions {
  return {
    waspExe: "wasp" as WaspCliExe,
    waspProjectDir: appWithNeitherDir,
    vercelExe: "vercel",
    db: "neon",
    serverSecret: [],
    ...overrides,
  };
}

const NEON_DATABASE_URL =
  "postgresql://user:pass@ep-fake-pooler.eu-central-1.aws.neon.tech/neondb?sslmode=require";
const NEON_DATABASE_URL_UNPOOLED =
  "postgresql://user:pass@ep-fake.eu-central-1.aws.neon.tech/neondb?sslmode=require";
const CUSTOM_DATABASE_URL = "postgresql://user:pass@db.example.com:5432/mydb";

describe("project names and URLs", () => {
  test("derives deterministic -server and -client project names", () => {
    expect(getServerProjectName("my-app")).toBe("my-app-server");
    expect(getClientProjectName("my-app")).toBe("my-app-client");
  });

  test("derives deterministic vercel.app URLs from the project names", () => {
    expect(getServerAppUrl("my-app")).toBe("https://my-app-server.vercel.app");
    expect(getClientAppUrl("my-app")).toBe("https://my-app-client.vercel.app");
  });
});

describe("assertVercelAppNameIsValid", () => {
  test("accepts lowercase names with digits, hyphens and dots", () => {
    expect(() => assertVercelAppNameIsValid("my-app-2")).not.toThrow();
    expect(() => assertVercelAppNameIsValid("app.v2")).not.toThrow();
  });

  test("rejects names with uppercase, spaces or a leading separator", () => {
    expect(() => assertVercelAppNameIsValid("MyApp")).toThrow();
    expect(() => assertVercelAppNameIsValid("my app")).toThrow();
    expect(() => assertVercelAppNameIsValid("-my-app")).toThrow();
    expect(() => assertVercelAppNameIsValid("")).toThrow();
  });

  test("rejects names too long to fit the -server/-client suffix", () => {
    expect(() => assertVercelAppNameIsValid("a".repeat(91))).toThrow();
    expect(() => assertVercelAppNameIsValid("a".repeat(90))).not.toThrow();
  });
});

describe("parseServerSecrets", () => {
  test("parses NAME=VALUE pairs", () => {
    expect(parseServerSecrets(["FOO=bar", "BAZ_2=qux"])).toEqual([
      { name: "FOO", value: "bar" },
      { name: "BAZ_2", value: "qux" },
    ]);
  });

  test("keeps '=' characters inside the value", () => {
    expect(parseServerSecrets(["FOO=a=b=c"])).toEqual([
      { name: "FOO", value: "a=b=c" },
    ]);
  });

  test("rejects entries without '=' or with an invalid name", () => {
    expect(() => parseServerSecrets(["FOO"])).toThrow();
    expect(() => parseServerSecrets(["=bar"])).toThrow();
    expect(() => parseServerSecrets(["1BAD=x"])).toThrow();
    expect(() => parseServerSecrets(["BAD-NAME=x"])).toThrow();
  });
});

describe("appUsesAuth", () => {
  test("detects the auth block in a legacy main.wasp", () => {
    expect(appUsesAuth(appWithAuthDir)).toBe(true);
  });

  test("detects auth(...) in a TS spec main.wasp.ts", () => {
    expect(appUsesAuth(appWithAuthTsDir)).toBe(true);
  });

  test("returns false for an app without auth", () => {
    expect(appUsesAuth(appWithNeitherDir)).toBe(false);
  });

  test("returns false when no Wasp entry file exists", () => {
    expect(appUsesAuth(fixturesDir as WaspProjectDir)).toBe(false);
  });
});

describe("parseEnvFile", () => {
  test("parses the dotenv format `vercel env pull` writes", () => {
    const contents = [
      "# Created by Vercel CLI",
      'DATABASE_URL="postgresql://u:p@ep-x-pooler.aws.neon.tech/db?sslmode=require"',
      'DATABASE_URL_UNPOOLED="postgresql://u:p@ep-x.aws.neon.tech/db?sslmode=require"',
      "PLAIN=unquoted-value",
      "",
      "not a valid line",
    ].join("\n");
    expect(parseEnvFile(contents)).toEqual({
      DATABASE_URL:
        "postgresql://u:p@ep-x-pooler.aws.neon.tech/db?sslmode=require",
      DATABASE_URL_UNPOOLED:
        "postgresql://u:p@ep-x.aws.neon.tech/db?sslmode=require",
      PLAIN: "unquoted-value",
    });
  });
});

describe("runSetup with --database-url (escape hatch)", () => {
  let cli: FakeVercelCli;

  beforeEach(() => {
    cli = new FakeVercelCli();
  });

  test("creates exactly the <app>-server and <app>-client projects", async () => {
    await runSetup(
      cli,
      "my-app",
      makeOptions({ databaseUrl: CUSTOM_DATABASE_URL }),
    );
    expect(cli.createdProjects).toEqual(["my-app-server", "my-app-client"]);
  });

  test("skips creating projects that already exist (idempotent)", async () => {
    cli.existingProjects.add("my-app-server");
    await runSetup(
      cli,
      "my-app",
      makeOptions({ databaseUrl: CUSTOM_DATABASE_URL }),
    );
    expect(cli.createdProjects).toEqual(["my-app-client"]);
  });

  test("sets DATABASE_URL verbatim and defaults DIRECT_URL to the same value", async () => {
    await runSetup(
      cli,
      "my-app",
      makeOptions({ databaseUrl: CUSTOM_DATABASE_URL }),
    );
    expect(cli.getEnvVar("my-app-server", "DATABASE_URL")).toEqual({
      value: CUSTOM_DATABASE_URL,
      environment: "production",
    });
    expect(cli.getEnvVar("my-app-server", "DIRECT_URL")?.value).toBe(
      CUSTOM_DATABASE_URL,
    );
  });

  test("uses --direct-url for DIRECT_URL when provided", async () => {
    await runSetup(
      cli,
      "my-app",
      makeOptions({
        databaseUrl: NEON_DATABASE_URL,
        directUrl: NEON_DATABASE_URL_UNPOOLED,
      }),
    );
    expect(cli.getEnvVar("my-app-server", "DIRECT_URL")?.value).toBe(
      NEON_DATABASE_URL_UNPOOLED,
    );
  });

  test("does not touch the Neon integration", async () => {
    await runSetup(
      cli,
      "my-app",
      makeOptions({ databaseUrl: CUSTOM_DATABASE_URL }),
    );
    expect(cli.addIntegrationResourceCalls).toEqual([]);
    expect(cli.connectIntegrationResourceCalls).toEqual([]);
  });

  test("sets WASP_SERVER_URL and WASP_WEB_CLIENT_URL on the server project", async () => {
    await runSetup(
      cli,
      "my-app",
      makeOptions({ databaseUrl: CUSTOM_DATABASE_URL }),
    );
    expect(cli.getEnvVar("my-app-server", "WASP_SERVER_URL")?.value).toBe(
      "https://my-app-server.vercel.app",
    );
    expect(cli.getEnvVar("my-app-server", "WASP_WEB_CLIENT_URL")?.value).toBe(
      "https://my-app-client.vercel.app",
    );
  });

  test("sets env vars on the server project only", async () => {
    await runSetup(
      cli,
      "my-app",
      makeOptions({ databaseUrl: CUSTOM_DATABASE_URL }),
    );
    expect(cli.getEnvVarNames("my-app-client")).toEqual([]);
  });

  test("sets JWT_SECRET to a random hex string when the app uses auth", async () => {
    await runSetup(
      cli,
      "my-app",
      makeOptions({
        databaseUrl: CUSTOM_DATABASE_URL,
        waspProjectDir: appWithAuthDir,
      }),
    );
    const jwtSecret = cli.getEnvVar("my-app-server", "JWT_SECRET");
    expect(jwtSecret?.value).toMatch(/^[0-9a-f]{64}$/);
  });

  test("a setup re-run keeps the same JWT_SECRET (skip-if-exists)", async () => {
    const options = makeOptions({
      databaseUrl: CUSTOM_DATABASE_URL,
      waspProjectDir: appWithAuthDir,
    });
    await runSetup(cli, "my-app", options);
    const firstSecret = cli.getEnvVar("my-app-server", "JWT_SECRET")?.value;
    await runSetup(cli, "my-app", options);
    const secondSecret = cli.getEnvVar("my-app-server", "JWT_SECRET")?.value;
    expect(firstSecret).toMatch(/^[0-9a-f]{64}$/);
    // Mirrors Railway skip-if-exists: re-running setup must not mint a fresh
    // JWT_SECRET (which would invalidate every previously issued session).
    expect(secondSecret).toBe(firstSecret);
  });

  test("does not set JWT_SECRET when the app has no auth", async () => {
    await runSetup(
      cli,
      "my-app",
      makeOptions({ databaseUrl: CUSTOM_DATABASE_URL }),
    );
    expect(cli.getEnvVar("my-app-server", "JWT_SECRET")).toBeUndefined();
  });

  test("passes --server-secret values through to the server project", async () => {
    await runSetup(
      cli,
      "my-app",
      makeOptions({
        databaseUrl: CUSTOM_DATABASE_URL,
        serverSecret: ["FOO=bar", "STRIPE_KEY=sk_test_123"],
      }),
    );
    expect(cli.getEnvVar("my-app-server", "FOO")?.value).toBe("bar");
    expect(cli.getEnvVar("my-app-server", "STRIPE_KEY")?.value).toBe(
      "sk_test_123",
    );
  });

  test("rejects malformed --server-secret values before changing anything", async () => {
    await expect(
      runSetup(
        cli,
        "my-app",
        makeOptions({
          databaseUrl: CUSTOM_DATABASE_URL,
          serverSecret: ["NO_EQUALS_SIGN"],
        }),
      ),
    ).rejects.toThrow();
    expect(cli.createdProjects).toEqual([]);
  });

  test("rejects an invalid app name before changing anything", async () => {
    await expect(
      runSetup(
        cli,
        "My Bad App",
        makeOptions({ databaseUrl: CUSTOM_DATABASE_URL }),
      ),
    ).rejects.toThrow();
    expect(cli.createdProjects).toEqual([]);
  });
});

describe("runSetup with --db neon (default)", () => {
  let cli: FakeVercelCli;

  beforeEach(() => {
    cli = new FakeVercelCli();
    cli.integrationInjectedEnvVars = {
      DATABASE_URL: NEON_DATABASE_URL,
      DATABASE_URL_UNPOOLED: NEON_DATABASE_URL_UNPOOLED,
    };
  });

  test("provisions a Neon resource named <app>-db connected to the server project", async () => {
    await runSetup(cli, "my-app", makeOptions());
    expect(cli.addIntegrationResourceCalls).toHaveLength(1);
    const call = cli.addIntegrationResourceCalls[0];
    expect(call.integrationSlug).toBe("neon");
    expect(call.resourceName).toBe("my-app-db");
    expect(cli.linkedDirs.get(call.linkedProjectDir)).toBe("my-app-server");
  });

  test("maps the injected DATABASE_URL_UNPOOLED to DIRECT_URL", async () => {
    await runSetup(cli, "my-app", makeOptions());
    expect(cli.getEnvVar("my-app-server", "DIRECT_URL")?.value).toBe(
      NEON_DATABASE_URL_UNPOOLED,
    );
  });

  test("stops with one-time consent instructions when Neon is not installed on the team", async () => {
    cli.neonInstalled = false;
    await expect(runSetup(cli, "my-app", makeOptions())).rejects.toThrow(
      /accept-terms neon/,
    );
    // Nothing was created: re-running after the one-time consent resumes cleanly.
    expect(cli.createdProjects).toEqual([]);
    expect(cli.addIntegrationResourceCalls).toEqual([]);
  });

  test("falls back to connecting an existing resource when provisioning reports it exists", async () => {
    cli.addIntegrationResourceResult = {
      success: false,
      output: "Error: resource with name my-app-db already exists",
    };
    await runSetup(cli, "my-app", makeOptions());
    expect(cli.connectIntegrationResourceCalls).toHaveLength(1);
    expect(cli.connectIntegrationResourceCalls[0]).toMatchObject({
      resourceName: "my-app-db",
      projectName: "my-app-server",
    });
  });

  test("fails with escape-hatch instructions when provisioning and connecting both fail", async () => {
    cli.addIntegrationResourceResult = {
      success: false,
      output: "Error: no billing plan selected",
    };
    cli.connectIntegrationResourceResult = {
      success: false,
      output: "Error: resource not found",
    };
    await expect(runSetup(cli, "my-app", makeOptions())).rejects.toThrow(
      /--database-url/,
    );
  });

  test("fails when the integration did not inject DATABASE_URL", async () => {
    cli.integrationInjectedEnvVars = {};
    await expect(runSetup(cli, "my-app", makeOptions())).rejects.toThrow(
      /DATABASE_URL/,
    );
  });

  test("rejects an unsupported --db value", async () => {
    await expect(
      runSetup(cli, "my-app", makeOptions({ db: "sqlite" })),
    ).rejects.toThrow(/--db/);
  });
});

describe("JWT_SECRET skip-if-exists (idempotent setup re-runs)", () => {
  let cli: FakeVercelCli;

  beforeEach(() => {
    cli = new FakeVercelCli();
  });

  test("sets a JWT_SECRET when the server project does not have one yet", async () => {
    const setEnvVarSpy = vi.spyOn(cli, "setEnvVar");
    await runSetup(
      cli,
      "my-app",
      makeOptions({
        databaseUrl: CUSTOM_DATABASE_URL,
        waspProjectDir: appWithAuthDir,
      }),
    );
    const jwtWrites = setEnvVarSpy.mock.calls.filter(
      (call) => call[1] === "JWT_SECRET",
    );
    expect(jwtWrites).toHaveLength(1);
    expect(cli.getEnvVar("my-app-server", "JWT_SECRET")?.value).toMatch(
      /^[0-9a-f]{64}$/,
    );
  });

  test("does NOT overwrite an already-set JWT_SECRET on a re-run", async () => {
    // A prior setup already provisioned the server project and its JWT_SECRET.
    cli.existingProjects.add("my-app-server");
    cli.envVars.set(
      "my-app-server",
      new Map([
        [
          "JWT_SECRET",
          { value: "pre-existing-secret", environment: "production" },
        ],
      ]),
    );

    const setEnvVarSpy = vi.spyOn(cli, "setEnvVar");
    await runSetup(
      cli,
      "my-app",
      makeOptions({
        databaseUrl: CUSTOM_DATABASE_URL,
        waspProjectDir: appWithAuthDir,
      }),
    );

    // The value must survive untouched and setEnvVar must never be called for
    // JWT_SECRET (regenerating it would invalidate all issued sessions).
    expect(cli.getEnvVar("my-app-server", "JWT_SECRET")?.value).toBe(
      "pre-existing-secret",
    );
    const jwtWrites = setEnvVarSpy.mock.calls.filter(
      (call) => call[1] === "JWT_SECRET",
    );
    expect(jwtWrites).toEqual([]);
  });
});

describe("runSetup output", () => {
  test("announces setup and mentions both created projects", async () => {
    const logSpy = vi.spyOn(console, "log").mockImplementation(() => undefined);
    try {
      const cli = new FakeVercelCli();
      await runSetup(
        cli,
        "my-app",
        makeOptions({ databaseUrl: CUSTOM_DATABASE_URL }),
      );
      const printed = logSpy.mock.calls.map((call) => call.join(" ")).join("\n");
      expect(printed).toContain("my-app-server");
      expect(printed).toContain("my-app-client");
    } finally {
      logSpy.mockRestore();
    }
  });
});
