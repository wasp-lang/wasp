import path from "node:path";
import { fileURLToPath } from "node:url";

import { beforeEach, describe, expect, test, vi } from "vitest";

// task-17: index.test.ts already asserts that --wasp-exe, --wasp-project-dir
// and --vercel-exe are *registered* as Commander options on every
// subcommand. It never asserts that argv values actually reach the
// provider's action handler (setup/deploy/launch in ./setup.js and
// ./deploy.js). This file pins that wiring directly: it mocks setup.js and
// deploy.js with spies (no real setup/deploy logic runs, no `vercel` binary
// is ever invoked for real work) and asserts the exact values parsed from
// argv show up in the spy's call args.
//
// vi.hoisted is required here: vi.mock factories are hoisted above the
// rest of the module, so a factory that closes over a later `const` would
// otherwise throw "Cannot access before initialization".
const { setupSpy, deploySpy } = vi.hoisted(() => ({
  setupSpy: vi.fn().mockResolvedValue(undefined),
  deploySpy: vi.fn().mockResolvedValue(undefined),
}));

vi.mock("../../../src/providers/vercel/setup.js", () => ({
  setup: setupSpy,
}));
vi.mock("../../../src/providers/vercel/deploy.js", () => ({
  deploy: deploySpy,
}));

import { createVercelCommand } from "../../../src/providers/vercel/index.js";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const fixturesDir = path.join(__dirname, "fixtures");
// Never actually executed: the preAction hooks only read --wasp-project-dir
// off disk (preflight scan) and shell out to --vercel-exe (CLI readiness
// check). --wasp-exe itself is only ever invoked by the real setup/deploy
// logic, which is mocked out below, so this path does not need to exist or
// be executable.
const waspExePath = path.join(fixturesDir, "not-a-real-wasp-cli");
// A real fixture dir with neither jobs nor webSockets, so the preflight
// hook's fs scan runs cleanly (no console.warn noise) without needing
// network access or a real Wasp project.
const waspProjectDir = path.join(fixturesDir, "appWithNeither");
// Stub `vercel` binary: exits 0 for any invocation (--version, whoami),
// so the CLI-readiness preAction hook is satisfied with no real Vercel
// CLI, no VERCEL_TOKEN and no network access.
const stubVercelCliPath = path.join(fixturesDir, "stubVercelCli.sh");

// createVercelCommand() mutates module-level singleton subcommands (see
// index.test.ts), so - exactly like that file - build it once and share it
// across every test below.
const vercel = createVercelCommand();

const argv = (subcommand: string, appName: string) => [
  subcommand,
  appName,
  "--wasp-exe",
  waspExePath,
  "--wasp-project-dir",
  waspProjectDir,
  "--vercel-exe",
  stubVercelCliPath,
];

describe("CLI arg wiring into the provider's action handler (task-17)", () => {
  beforeEach(() => {
    setupSpy.mockClear();
    deploySpy.mockClear();
  });

  test("setup subcommand: --wasp-exe/--wasp-project-dir/--vercel-exe reach setup()", async () => {
    await vercel.parseAsync(argv("setup", "wiring-app"), { from: "user" });

    expect(setupSpy).toHaveBeenCalledTimes(1);
    expect(deploySpy).not.toHaveBeenCalled();

    const [appNameArg, optionsArg] = setupSpy.mock.calls[0];
    expect(appNameArg).toBe("wiring-app");
    expect(optionsArg).toMatchObject({
      waspExe: waspExePath,
      waspProjectDir,
      vercelExe: stubVercelCliPath,
    });
  });

  test("deploy subcommand: --wasp-exe/--wasp-project-dir/--vercel-exe reach deploy()", async () => {
    await vercel.parseAsync(argv("deploy", "wiring-app"), { from: "user" });

    expect(deploySpy).toHaveBeenCalledTimes(1);
    expect(setupSpy).not.toHaveBeenCalled();

    const [appNameArg, optionsArg] = deploySpy.mock.calls[0];
    expect(appNameArg).toBe("wiring-app");
    expect(optionsArg).toMatchObject({
      waspExe: waspExePath,
      waspProjectDir,
      vercelExe: stubVercelCliPath,
    });
  });

  test("launch subcommand: same values reach both setup() and deploy() (launch = setup then deploy)", async () => {
    await vercel.parseAsync(argv("launch", "wiring-app"), { from: "user" });

    expect(setupSpy).toHaveBeenCalledTimes(1);
    expect(deploySpy).toHaveBeenCalledTimes(1);

    for (const spy of [setupSpy, deploySpy]) {
      const [appNameArg, optionsArg] = spy.mock.calls[0];
      expect(appNameArg).toBe("wiring-app");
      expect(optionsArg).toMatchObject({
        waspExe: waspExePath,
        waspProjectDir,
        vercelExe: stubVercelCliPath,
      });
    }
  });

  test("a different app name and CLI paths are threaded through verbatim (not hardcoded)", async () => {
    const otherWaspExe = path.join(fixturesDir, "some-other-wasp-exe");
    await vercel.parseAsync(
      [
        "deploy",
        "another-app",
        "--wasp-exe",
        otherWaspExe,
        "--wasp-project-dir",
        waspProjectDir,
        "--vercel-exe",
        stubVercelCliPath,
      ],
      { from: "user" },
    );

    const [appNameArg, optionsArg] = deploySpy.mock.calls[0];
    expect(appNameArg).toBe("another-app");
    expect(optionsArg).toMatchObject({ waspExe: otherWaspExe });
  });
});
