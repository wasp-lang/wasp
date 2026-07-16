import path from "node:path";
import { fileURLToPath } from "node:url";

import { afterEach, beforeEach, describe, expect, test, vi } from "vitest";

import { WaspProjectDir } from "../../../src/common/brandedTypes.js";
import {
  buildPreflightWarnings,
  runVercelPreflightChecks,
  scanWaspProjectForPreflightFindings,
} from "../../../src/providers/vercel/preflight.js";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const fixturesDir = path.join(__dirname, "fixtures");

function fixture(name: string): WaspProjectDir {
  return path.join(fixturesDir, name) as WaspProjectDir;
}

describe("scanWaspProjectForPreflightFindings", () => {
  test("detects a job declared directly in main.wasp.ts", () => {
    const findings = scanWaspProjectForPreflightFindings(
      fixture("appWithJob"),
    );
    expect(findings.jobNames).toEqual(["sendEmailJob"]);
    expect(findings.hasWebSocket).toBe(false);
  });

  test("detects jobs declared in a file imported from main.wasp.ts", () => {
    const findings = scanWaspProjectForPreflightFindings(
      fixture("appWithJobsAcrossFiles"),
    );
    expect(findings.jobNames.sort()).toEqual(
      ["mySpecialJob", "uppercaseTextJob"].sort(),
    );
    expect(findings.hasWebSocket).toBe(false);
  });

  test("detects webSocket usage in main.wasp.ts", () => {
    const findings = scanWaspProjectForPreflightFindings(
      fixture("appWithWebSocket"),
    );
    expect(findings.hasWebSocket).toBe(true);
    expect(findings.jobNames).toEqual([]);
  });

  test("detects nothing for a project with neither jobs nor webSockets", () => {
    const findings = scanWaspProjectForPreflightFindings(
      fixture("appWithNeither"),
    );
    expect(findings.jobNames).toEqual([]);
    expect(findings.hasWebSocket).toBe(false);
  });

  test("returns empty findings when no main.wasp/main.wasp.ts is present", () => {
    const findings = scanWaspProjectForPreflightFindings(
      fixture("doesNotExist"),
    );
    expect(findings.jobNames).toEqual([]);
    expect(findings.hasWebSocket).toBe(false);
  });
});

describe("buildPreflightWarnings", () => {
  test("AC #1: names the specific job(s) and mentions Vercel Cron as mitigation", () => {
    const warnings = buildPreflightWarnings({
      jobNames: ["sendEmailJob", "cleanupJob"],
      hasWebSocket: false,
    });

    expect(warnings).toHaveLength(1);
    const [warning] = warnings;
    expect(warning.message).toMatch(/WARN/);
    expect(warning.message).toContain("sendEmailJob");
    expect(warning.message).toContain("cleanupJob");
    expect(warning.message).toContain("Vercel Cron");
    expect(warning.message.toLowerCase()).toContain("scale-to-zero");
  });

  test("AC #2: names WebSockets and notes beta/instance-pinned/maxDuration caveats", () => {
    const warnings = buildPreflightWarnings({
      jobNames: [],
      hasWebSocket: true,
    });

    expect(warnings).toHaveLength(1);
    const [warning] = warnings;
    expect(warning.message).toMatch(/WARN/);
    expect(warning.message).toContain("WebSocket");
    expect(warning.message.toLowerCase()).toContain("beta");
    expect(warning.message.toLowerCase()).toContain("instance-pinned");
    expect(warning.message.toLowerCase()).toContain("maxduration");
  });

  test("AC #3: no warnings when neither jobs nor webSockets are present", () => {
    const warnings = buildPreflightWarnings({
      jobNames: [],
      hasWebSocket: false,
    });
    expect(warnings).toEqual([]);
  });

  test("emits both warnings when both jobs and webSockets are present", () => {
    const warnings = buildPreflightWarnings({
      jobNames: ["sendEmailJob"],
      hasWebSocket: true,
    });
    expect(warnings).toHaveLength(2);
  });
});

describe("runVercelPreflightChecks", () => {
  let warnSpy: ReturnType<typeof vi.spyOn>;

  beforeEach(() => {
    warnSpy = vi.spyOn(console, "warn").mockImplementation(() => undefined);
  });

  afterEach(() => {
    warnSpy.mockRestore();
  });

  test("prints a job warning naming the job and mentioning Vercel Cron, and does not throw", () => {
    expect(() =>
      runVercelPreflightChecks(fixture("appWithJob")),
    ).not.toThrow();

    const printed = warnSpy.mock.calls.map((call) => call.join(" ")).join("\n");
    expect(printed).toContain("sendEmailJob");
    expect(printed).toContain("Vercel Cron");
  });

  test("prints a webSocket warning and does not throw", () => {
    expect(() =>
      runVercelPreflightChecks(fixture("appWithWebSocket")),
    ).not.toThrow();

    const printed = warnSpy.mock.calls.map((call) => call.join(" ")).join("\n");
    expect(printed).toContain("WebSocket");
  });

  test("prints nothing and does not throw when neither is declared", () => {
    expect(() =>
      runVercelPreflightChecks(fixture("appWithNeither")),
    ).not.toThrow();

    expect(warnSpy).not.toHaveBeenCalled();
  });

  test("never throws even when the project dir/entry file is missing (warn-not-block)", () => {
    expect(() =>
      runVercelPreflightChecks(fixture("doesNotExist")),
    ).not.toThrow();
    expect(warnSpy).not.toHaveBeenCalled();
  });
});
