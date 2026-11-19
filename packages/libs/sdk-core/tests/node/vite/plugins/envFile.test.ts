import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { afterEach, beforeEach, describe, expect, it } from "vitest";
import { loadEnvVars } from "../../../../src/node/vite/plugins/envFile.js";

describe("loadEnvVars", () => {
  let tempDir: string;

  beforeEach(async () => {
    tempDir = await mkdtemp(join(tmpdir(), "wasp-envfile-test-"));
  });

  afterEach(async () => {
    await rm(tempDir, { recursive: true, force: true });
  });

  it("keeps the last definition when the same variable is defined twice", async () => {
    await writeFile(
      join(tempDir, ".env.client"),
      "REACT_APP_SOME_VAR=1\nREACT_APP_SOME_VAR=2\n",
      "utf-8",
    );

    const result = await loadEnvVars({
      rootDir: tempDir,
      envFileName: ".env.client",
      envPrefix: "REACT_APP_",
      loadDotEnvFile: true,
    });

    expect(result.REACT_APP_SOME_VAR).toBe("2");
  });

  it("keeps ${VAR} and $(command) literally without expanding or executing them", async () => {
    await writeFile(
      join(tempDir, ".env.client"),
      [
        "REACT_APP_BASE=hello",
        "REACT_APP_INTERPOLATED=${REACT_APP_BASE}",
        "REACT_APP_COMMAND=$(echo hi)",
      ].join("\n"),
      "utf-8",
    );

    const result = await loadEnvVars({
      rootDir: tempDir,
      envFileName: ".env.client",
      envPrefix: "REACT_APP_",
      loadDotEnvFile: true,
    });

    expect(result.REACT_APP_BASE).toBe("hello");
    expect(result.REACT_APP_INTERPOLATED).toBe("${REACT_APP_BASE}");
    expect(result.REACT_APP_COMMAND).toBe("$(echo hi)");
  });

  it("handles unquoted spaced values, backtick quotes, non-definition lines, escaped quotes, and prefix filtering", async () => {
    await writeFile(
      join(tempDir, ".env.client"),
      [
        "not a definition",
        "REACT_APP_SPACED=some spaced value",
        "REACT_APP_BACKTICK=`value`",
        'REACT_APP_ESCAPED_QUOTE="a\\"b"',
        "UNPREFIXED_VAR=ignored",
      ].join("\n"),
      "utf-8",
    );

    const result = await loadEnvVars({
      rootDir: tempDir,
      envFileName: ".env.client",
      envPrefix: "REACT_APP_",
      loadDotEnvFile: true,
    });

    expect(result.REACT_APP_SPACED).toBe("some spaced value");
    expect(result.REACT_APP_BACKTICK).toBe("value");
    expect(result.REACT_APP_ESCAPED_QUOTE).toBe('a\\"b');
    expect(result.UNPREFIXED_VAR).toBeUndefined();
  });

  it("ignores the dotenv file when loadDotEnvFile is false", async () => {
    await writeFile(
      join(tempDir, ".env.client"),
      "REACT_APP_SOME_VAR=from_file\n",
      "utf-8",
    );

    const result = await loadEnvVars({
      rootDir: tempDir,
      envFileName: ".env.client",
      envPrefix: "REACT_APP_",
      loadDotEnvFile: false,
    });

    expect(result.REACT_APP_SOME_VAR).toBeUndefined();
  });
});
