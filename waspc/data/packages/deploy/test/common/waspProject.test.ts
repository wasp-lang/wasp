import { describe, expect, test } from "vitest";
import { parseWaspInfoError } from "../../src/common/waspProject.js";

describe("parseWaspInfoError", () => {
  test("returns version mismatch message when output contains version mismatch", () => {
    const output = [
      "Your Wasp version does not match the app's requirements.",
      "You are running Wasp 0.13.0.",
      "This app requires Wasp ^0.14.0.",
      "To install a specific version of Wasp, do:",
      "  npm i -g @wasp.sh/wasp-cli@x.y.z",
      "where x.y.z is your desired version.",
      "Check https://github.com/wasp-lang/wasp/releases for the list of valid versions.",
    ].join("\n");

    const result = parseWaspInfoError(output);

    expect(result).toContain("Wasp version mismatch detected.");
    expect(result).toContain(
      "Your Wasp version does not match the app's requirements.",
    );
    expect(result).toContain("You are running Wasp 0.13.0.");
    expect(result).toContain("This app requires Wasp ^0.14.0.");
    expect(result).toContain(
      "Please install the correct version of Wasp before deploying.",
    );
    expect(result).not.toContain("does not appear to be a valid Wasp project");
  });

  test("extracts relevant lines from noisy output", () => {
    const output = [
      "Some other output before",
      "",
      "Your Wasp version does not match the app's requirements.",
      "You are running Wasp 0.11.0.",
      "This app requires Wasp ^0.12.0.",
      "To install a specific version of Wasp, do:",
      "  npm i -g @wasp.sh/wasp-cli@x.y.z",
    ].join("\n");

    const result = parseWaspInfoError(output);

    expect(result).toContain("Wasp version mismatch detected.");
    expect(result).toContain("You are running Wasp 0.11.0.");
    expect(result).toContain("This app requires Wasp ^0.12.0.");
    expect(result).not.toContain("Some other output before");
    expect(result).not.toContain("npm i -g");
  });

  test("returns generic error when output has no version mismatch", () => {
    const output = "Some unrelated error occurred";

    const result = parseWaspInfoError(output);

    expect(result).toContain(
      "The supplied Wasp directory does not appear to be a valid Wasp project.",
    );
    expect(result).toContain(
      "Please double check your Wasp project directory.",
    );
    expect(result).not.toContain("version mismatch");
  });

  test("returns generic error for empty output", () => {
    const result = parseWaspInfoError("");

    expect(result).toContain(
      "The supplied Wasp directory does not appear to be a valid Wasp project.",
    );
  });

  test("handles version mismatch with leading/trailing whitespace in lines", () => {
    const output = [
      "  Your Wasp version does not match the app's requirements.  ",
      "  You are running Wasp 0.15.0.  ",
      "  This app requires Wasp ^0.16.0.  ",
    ].join("\n");

    const result = parseWaspInfoError(output);

    expect(result).toContain("Wasp version mismatch detected.");
    expect(result).toContain("You are running Wasp 0.15.0.");
    expect(result).toContain("This app requires Wasp ^0.16.0.");
  });
});
