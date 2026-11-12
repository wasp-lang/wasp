import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { createDummyEmailSender } from "../src/dummyEmailSender";

const EMAIL = {
  to: "user@app.test",
  subject: "Verify your email",
  text: "Verify at https://app.test/verify",
  html: `<a href="https://app.test/verify">Verify</a>`,
};

let lines: string[];

beforeEach(() => {
  lines = [];
});

afterEach(() => {
  vi.unstubAllEnvs();
});

function createSender() {
  return createDummyEmailSender({
    defaultFrom: { name: "App", email: "app@app.test" },
    log: (line) => lines.push(line),
  });
}

describe("createDummyEmailSender", () => {
  it("logs the email instead of sending it", async () => {
    await expect(createSender().send(EMAIL)).resolves.toEqual({
      success: true,
    });

    expect(lines).toContain("To:      user@app.test");
    expect(lines).toContain("Subject: Verify your email");
    expect(lines).toContain(EMAIL.text);
    expect(lines).toContain(EMAIL.html);
  });

  it("uses the default sender when the email has none", async () => {
    await createSender().send(EMAIL);

    expect(lines).toContain("From:    App <app@app.test>");
  });

  it("uses the email's sender when it has one", async () => {
    await createSender().send({ ...EMAIL, from: { email: "other@app.test" } });

    expect(lines).toContain("From:    other@app.test");
  });

  it("colors the banner", async () => {
    vi.stubEnv("NO_COLOR", undefined);

    await createSender().send(EMAIL);

    expect(lines[0]).toMatch(/^\x1b\[33m/);
  });

  it("doesn't color anything when NO_COLOR is set", async () => {
    vi.stubEnv("NO_COLOR", "1");

    await createSender().send(EMAIL);

    expect(lines.some((line) => line.includes("\x1b["))).toBe(false);
  });
});
