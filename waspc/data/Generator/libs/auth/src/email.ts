/**
 * Our current email validation is a compromise between complexity
 * and corectness. This allows for both the server and the client
 * to validate the email in the same manner.
 *
 * If we want to have more complete email validation (like RFC 6531),
 * we should opt out for a library instead, and most likely delegate
 * the complete email validation to server only.
 *
 * Most other frameworks either:
 * - Implement the full (or close to full) email specification and
 *   validate server side.
 * - Do almost zero validation (simple "@" check).
 *
 * The shared idea is to be the most accessible you can be, since
 * invalid emails will be blocked by the email verification step anyway.
 */

/**
 * Accepts HTML5 `input[type=email]` grammar syntax, widened to also
 * accept most Unicode characters (HTML5 is ASCII only), while disallowing
 * possibly malicious emails.
 *
 * It only accepts Unicode letters, marks and decimal digits, leaving out
 * the possibly dangerous format characters.
 *
 * @see {@link isPossiblyMaliciousEmail} for more details about the safety checks.
 * @see {@link https://github.com/whatwg/html/issues/4562 WHATWG international email addresses issue}
 */
export function isValidEmail(input: unknown): boolean {
  if (typeof input !== "string") {
    return false;
  }

  return (
    HTML5_EMAIL_WITH_UNICODE_REGEX.test(input) &&
    !isPossiblyMaliciousEmail(input) &&
    isEmailOfValidLength(input)
  );
}

const HTML5_EMAIL_WITH_UNICODE_REGEX =
  /^[\p{L}\p{M}\p{Nd}.!#$%&'*+/=?^_`{|}~-]+@[\p{L}\p{Nd}](?:[\p{L}\p{M}\p{Nd}-]{0,61}[\p{L}\p{M}\p{Nd}])?(?:\.[\p{L}\p{Nd}](?:[\p{L}\p{M}\p{Nd}-]{0,61}[\p{L}\p{M}\p{Nd}])?)*$/u;

/**
 * Blocks possibily malicious patterns in regex.
 *
 * A large number of Unicode characters are not safe to display,
 * either by themsleves or in combination with surrounding text.
 *
 * @see {@link https://github.com/JoshData/python-email-validator#unsafe-unicode-characters-are-rejected}
 *      for more details, and is also what we were inspired by.
 */
function isPossiblyMaliciousEmail(email: string) {
  return (
    INVISIBLE_CHARACTER_REGEX.test(email) ||
    LEADING_COMBINING_MARK_REGEX.test(email)
  );
}

/**
 * Checks for characters that are invisible.
 *
 * An invisible character can be inserted into an email address without
 * changing how it appears when rendered. This can make two differently
 * spelled email addresses look identical.
 */
const INVISIBLE_CHARACTER_REGEX = /\p{Default_Ignorable_Code_Point}/u;

/**
 * Checks for a combining mark at the start.
 *
 * Combining marks are rendered together with a preceding character.
 * If an email address starts with one, it can combine visually with
 * the character before the address, potentially making the address
 * appear different from its actual spelling.
 */
const LEADING_COMBINING_MARK_REGEX = /^\p{M}/u;

/**
 * Checks whether an email address is within the maximum length limits.
 *
 * The upper bounds come from RFC 5321:
 * - 64 octets for the local part
 * - 254 octets for the entire email address.
 *
 * Octets are counted directly from the Unicode string.
 * Punycode (converting Unicode to ASCII) is not applied to the domain
 * for simplicity.
 */
function isEmailOfValidLength(email: string) {
  return (
    countOctets(email) <= MAX_EMAIL_ADDRESS_OCTETS &&
    countOctets(getEmailLocalPart(email)) <= MAX_EMAIL_ADDRESS_LOCAL_PART_OCTETS
  );
}

const MAX_EMAIL_ADDRESS_LOCAL_PART_OCTETS = 64;
const MAX_EMAIL_ADDRESS_OCTETS = 254;

function countOctets(text: string): number {
  return new TextEncoder().encode(text).length;
}

function getEmailLocalPart(email: string): string {
  return email.slice(0, email.lastIndexOf("@"));
}
