import path from "node:path";
import { fileURLToPath } from "node:url";

import { Command } from "commander";
import { afterEach, beforeEach, describe, expect, test, vi } from "vitest";
import { createVercelCommand } from "../../../src/providers/vercel/index.js";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const fixturesDir = path.join(__dirname, "fixtures");
const stubVercelCliPath = path.join(fixturesDir, "stubVercelCli.sh");
const stubFailingWaspCliPath = path.join(fixturesDir, "stubFailingWaspCli.sh");

// createVercelCommand() mutates module-level singleton subcommands
// (vercelSetupCommand/vercelDeployCommand/vercelLaunchCommand from
// task-13), so it can only safely be called once per process - build it a
// single time here and share it across every describe block below.
const vercel = createVercelCommand();

describe("createVercelCommand", () => {
  test("returns a command named 'vercel'", () => {
    expect(vercel).toBeInstanceOf(Command);
    expect(vercel.name()).toBe("vercel");
  });

  test("registers setup, deploy and launch subcommands", () => {
    const subcommandNames = vercel.commands.map((cmd) => cmd.name());
    expect(subcommandNames).toEqual(
      expect.arrayContaining(["setup", "deploy", "launch"]),
    );
  });

  describe.each(["setup", "deploy", "launch"])("%s subcommand", (name) => {
    const subcommand = vercel.commands.find((cmd) => cmd.name() === name);

    test("exists", () => {
      expect(subcommand).toBeDefined();
    });

    test("accepts hidden --wasp-exe option", () => {
      const option = subcommand?.options.find(
        (opt) => opt.long === "--wasp-exe",
      );
      expect(option).toBeDefined();
      expect(option?.hidden).toBe(true);
    });

    test("accepts hidden --wasp-project-dir option", () => {
      const option = subcommand?.options.find(
        (opt) => opt.long === "--wasp-project-dir",
      );
      expect(option).toBeDefined();
      expect(option?.hidden).toBe(true);
    });

    test("accepts --vercel-exe option defaulting to 'vercel'", () => {
      const option = subcommand?.options.find(
        (opt) => opt.long === "--vercel-exe",
      );
      expect(option).toBeDefined();
      expect(option?.defaultValue).toBe("vercel");
    });

    test("accepts --token and --scope auth options", () => {
      const longFlags = subcommand?.options.map((opt) => opt.long);
      expect(longFlags).toContain("--token");
      expect(longFlags).toContain("--scope");
    });
  });

  describe.each(["setup", "launch"])(
    "%s subcommand database options",
    (name) => {
      const subcommand = vercel.commands.find((cmd) => cmd.name() === name);

      test("accepts --db defaulting to neon, plus the --database-url escape hatch", () => {
        const longFlags = subcommand?.options.map((opt) => opt.long);
        expect(longFlags).toContain("--db");
        expect(longFlags).toContain("--database-url");
        expect(longFlags).toContain("--direct-url");
        const dbOption = subcommand?.options.find(
          (opt) => opt.long === "--db",
        );
        expect(dbOption?.defaultValue).toBe("neon");
      });
    },
  );
});

describe("preflight warnings wired into command run (task-16)", () => {
  let warnSpy: ReturnType<typeof vi.spyOn>;

  beforeEach(() => {
    warnSpy = vi.spyOn(console, "warn").mockImplementation(() => undefined);
  });

  afterEach(() => {
    warnSpy.mockRestore();
  });

  // The deploy/launch subcommands' real actions (task-15) run here against
  // stub CLIs: `wasp build` is a stub that always fails fast (deploy), and
  // the neon integration check comes back empty (launch), so both commands
  // reject after the preAction hooks ran. That rejection proves the
  // preflight warning did NOT block the command: the hook ran, printed its
  // warning, and let the real action run next - warn-not-block.
  describe.each(["deploy", "launch"])("%s subcommand", (name) => {
    test("prints the job preflight warning before the action's own failure", async () => {
      await expect(
        vercel.parseAsync(
          [
            name,
            "my-app",
            "--wasp-exe",
            stubFailingWaspCliPath,
            "--wasp-project-dir",
            path.join(fixturesDir, "appWithJob"),
            "--vercel-exe",
            stubVercelCliPath,
          ],
          { from: "user" },
        ),
      ).rejects.toThrow();

      const printed = warnSpy.mock.calls
        .map((call) => call.join(" "))
        .join("\n");
      expect(printed).toContain("sendEmailJob");
      expect(printed).toContain("Vercel Cron");
    });

    test("prints no preflight warning for a project with neither jobs nor webSockets", async () => {
      await expect(
        vercel.parseAsync(
          [
            name,
            "my-app",
            "--wasp-exe",
            stubFailingWaspCliPath,
            "--wasp-project-dir",
            path.join(fixturesDir, "appWithNeither"),
            "--vercel-exe",
            stubVercelCliPath,
          ],
          { from: "user" },
        ),
      ).rejects.toThrow();

      expect(warnSpy).not.toHaveBeenCalled();
    });
  });

  // The setup subcommand has a real action (task-14). Run it against the
  // stub Vercel CLI (every call exits 0) with the --database-url escape
  // hatch: it must print the preflight warning and then complete without
  // throwing - warn-not-block, end to end.
  describe("setup subcommand (real action, task-14)", () => {
    test("prints the job preflight warning and still completes", async () => {
      await expect(
        vercel.parseAsync(
          [
            "setup",
            "my-app",
            "--wasp-exe",
            "wasp",
            "--wasp-project-dir",
            path.join(fixturesDir, "appWithJob"),
            "--vercel-exe",
            stubVercelCliPath,
            "--database-url",
            "postgresql://user:pass@db.example.com:5432/mydb",
          ],
          { from: "user" },
        ),
      ).resolves.toBeDefined();

      const printed = warnSpy.mock.calls
        .map((call) => call.join(" "))
        .join("\n");
      expect(printed).toContain("sendEmailJob");
      expect(printed).toContain("Vercel Cron");
    });
  });
});
