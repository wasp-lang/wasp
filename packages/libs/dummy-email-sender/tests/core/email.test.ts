import { describe, expect, it } from "vitest";

import { createEmail, extractLinks } from "../../src/core/email";

describe("createEmail", () => {
  it("derives the id from the send time", () => {
    const email = createEmail(
      {
        from: { email: "a@app.test" },
        to: "b@app.test",
        subject: "",
        text: "",
        html: "",
      },
      new Date("2026-11-11T10:20:30.123Z"),
    );

    expect(email.id).toMatch(/^2026-11-11T10-20-30-123Z-[0-9a-f]+$/);
    expect(email.sentAt).toBe("2026-11-11T10:20:30.123Z");
  });
});

describe("extractLinks", () => {
  it("finds links in the text body without trailing punctuation", () => {
    expect(
      extractLinks({
        text: "Verify at https://app.test/verify?token=abc. Or (https://app.test/help).",
        html: "",
      }),
    ).toEqual(["https://app.test/verify?token=abc", "https://app.test/help"]);
  });

  it("finds hrefs in the HTML body and decodes entities", () => {
    expect(
      extractLinks({
        text: "",
        html: `<a href="https://app.test/reset?token=abc&amp;user=1">Reset</a> <a href='mailto:a@b.c'>Mail</a>`,
      }),
    ).toEqual(["https://app.test/reset?token=abc&user=1"]);
  });

  it("lists a link found in both bodies once", () => {
    expect(
      extractLinks({
        text: "https://app.test/verify",
        html: `<a href="https://app.test/verify">Verify</a>`,
      }),
    ).toEqual(["https://app.test/verify"]);
  });
});
