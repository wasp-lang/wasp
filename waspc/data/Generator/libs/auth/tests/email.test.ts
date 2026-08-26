import { describe, expect, it } from "vitest";
import { isValidEmail } from "../src/email";

describe("isValidEmail", () => {
  it.each([
    "user@example.com",
    "user.name+tag@example.co.uk",
    "user_name@example.com",
    "!#$%&'*+-/=?^_`{|}~@example.com",
    "user@sub.domain.example.com",
    "user@a.io",
    // `input[type=email]` accepts a dotless domain, so we do too.
    "user@localhost",
  ])("accepts the ASCII address %j", (email) => {
    expect(isValidEmail(email)).toBe(true);
  });

  it.each([
    "",
    "plainaddress",
    "@example.com",
    "user@",
    "user@@example.com",
    "user@-example.com",
    "user@example-.com",
    "user@exam ple.com",
    "user name@example.com",
    "user@example..com",
  ])("rejects the malformed address %j", (email) => {
    expect(isValidEmail(email)).toBe(false);
  });

  it.each([null, undefined, 42, {}, ["user@example.com"]])(
    "rejects the non-string input %j",
    (input) => {
      expect(isValidEmail(input)).toBe(false);
    },
  );

  describe("internationalized addresses", () => {
    it.each([
      "jürgen@example.com",
      "user@münchen.de",
      "jürgen@münchen.de",
      "用户@例子.广告",
      "θσερ@εχαμπλε.ψομ",
      "अजय@डाटा.भारत",
      "квіточка@пошта.укр",
      // Decomposed "ö", a combining mark following its base character.
      "o\u0308ffentlich@example.com",
      // Arabic-Indic digits.
      "١٢@example.com",
    ])("accepts %j", (email) => {
      expect(isValidEmail(email)).toBe(true);
    });

    // RFC 6531 allows any non-ASCII character in the local part.
    // We do not go that far.
    it.each([
      // Emoji.
      "😀@example.com",
      // Numerals that are not decimal digits.
      "user@examp⑪le.com",
      "user@Ⅷ.com",
    ])("rejects the unsupported character in %j", (email) => {
      expect(isValidEmail(email)).toBe(false);
    });

    it("rejects an address starting with a combining mark", () => {
      // The mark has no character of its own to attach to, so it lands on
      // whatever text precedes the address when it is rendered.
      expect(isValidEmail("\u0301user@example.com")).toBe(false);
    });
  });

  // These render as nothing, so they let two addresses that are spelled
  // differently look identical.
  describe("invisible characters", () => {
    it.each([
      // Zero width joiner.
      "us\u200Der@example.com",
      // Zero width non-joiner.
      "us\u200Cer@example.com",
      // Right-to-left override.
      "us\u202Eer@example.com",
      // Soft hyphen.
      "us\u00ADer@example.com",
      // Variation selector 1.
      "us\uFE00er@example.com",
      // Variation selector 16.
      "us\uFE0Fer@example.com",
      // Variation selector supplement.
      "us\u{E0100}er@example.com",
      // Mongolian free variation selector.
      "us\u180Ber@example.com",
      // Hangul filler, the classic invisible username character.
      "\u3164@example.com",
      // Halfwidth Hangul filler.
      "\uFFA0@example.com",
      // Hangul choseong filler, in the domain this time.
      "user@examp\u115Fle.com",
    ])("rejects %j", (email) => {
      expect(isValidEmail(email)).toBe(false);
    });
  });

  describe("case", () => {
    // The email signup endpoint validates the raw request body and only
    // lowercases the address afterwards, so the validator has to accept
    // whatever casing the user typed.
    it.each([
      "JOHN@EXAMPLE.COM",
      "John@Example.com",
      "jOhN@eXaMpLe.CoM",
      // Uppercase confined to the domain.
      "john@Example.com",
      "john@example.COM",
      // Uppercase confined to the local part, running right up to the "@".
      "JOHN@example.com",
      "john.DOE@example.com",
    ])("accepts %j", (email) => {
      expect(isValidEmail(email)).toBe(true);
    });
  });

  describe("anchoring", () => {
    it.each([
      "user@example.com <script>alert(1)</script>",
      "I am not an email, ask user@example.com",
      "\nuser@example.com",
      "user@example.com\n",
      " user@example.com ",
    ])("rejects %j, which merely contains an address", (input) => {
      expect(isValidEmail(input)).toBe(false);
    });
  });

  describe("size limits (RFC 5321)", () => {
    it("accepts a 64 octet local part", () => {
      expect(isValidEmail(`${"a".repeat(64)}@example.com`)).toBe(true);
    });

    it("rejects a 65 octet local part", () => {
      expect(isValidEmail(`${"a".repeat(65)}@example.com`)).toBe(false);
    });

    it("counts octets rather than characters in the local part", () => {
      // "ä" is two octets in UTF-8, so 33 of them exceed the 64 octet limit
      // while staying well under 64 characters.
      expect(isValidEmail(`${"ä".repeat(32)}@example.com`)).toBe(true);
      expect(isValidEmail(`${"ä".repeat(33)}@example.com`)).toBe(false);
    });

    it("rejects an address longer than 254 octets", () => {
      const domain = `${"a".repeat(61)}.${"b".repeat(61)}.${"c".repeat(61)}.com`;
      const localPart = "d".repeat(254 - domain.length - 1);

      expect(isValidEmail(`${localPart}@${domain}`)).toBe(true);
      expect(isValidEmail(`${localPart}x@${domain}`)).toBe(false);
    });
  });
});
