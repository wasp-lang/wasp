import { describe, expect, it } from "vitest";
import { mergeDefaultAndUserConfig } from "../src/config";

describe("config helpers", () => {
  it("should return default config when user config is missing", () => {
    const defaultConfig = { scopes: ["profile"], clientId: "default" };

    expect(mergeDefaultAndUserConfig(defaultConfig)).toBe(defaultConfig);
  });

  it("should merge default and user config", () => {
    expect(
      mergeDefaultAndUserConfig(
        { scopes: ["profile"], clientId: "default" },
        () => ({ clientId: "user", extra: true }),
      ),
    ).toEqual({
      scopes: ["profile"],
      clientId: "user",
      extra: true,
    });
  });
});
