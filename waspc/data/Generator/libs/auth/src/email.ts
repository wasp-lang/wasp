// TODO: If we ever need a more quality email validator, its worth to
// look at https://github.com/JoshData/python-email-validator for inspiration.

/**
 * The syntax we accept is the HTML5 `input[type=email]` grammar,
 * widened to also accept most unicode characters (HTML5 is ASCII only).
 *
 * We start from the HTML5 grammar because it is a good compromise
 * between completeness and complexity.
 *
 * We also do some additional safety checks:
 * - We disallow invisible, text-reordering, zero-width and bidirectional
 *   override (LTR or RTL) unicode characters.
 * - We disallow a combining mark at the start of an email address,
 *   because it would attach to whatever text that precedes the address.
 *
 * @see {@link https://github.com/whatwg/html/issues/4562 WHATWG international email addresses issue}
 */
const HTML5_EMAIL_WITH_UNICODE_REGEX =
  /^(?!\p{M})[\p{L}\p{M}\p{N}.!#$%&'*+/=?^_`{|}~-]+@[\p{L}\p{N}](?:[\p{L}\p{M}\p{N}-]{0,61}[\p{L}\p{M}\p{N}])?(?:\.[\p{L}\p{N}](?:[\p{L}\p{M}\p{N}-]{0,61}[\p{L}\p{M}\p{N}])?)*$/u;

export function isValidEmail(input: unknown): boolean {
  if (typeof input !== "string") {
    return false;
  }

  return (
    HTML5_EMAIL_WITH_UNICODE_REGEX.test(input) && isEmailOfValidLength(input)
  );
}

// Upper bounds from RFC 5321.
const MAX_EMAIL_ADDRESS_LOCAL_PART_OCTETS = 64;
const MAX_EMAIL_ADDRESS_OCTETS = 254;

function isEmailOfValidLength(email: string) {
  return (
    countOctets(email) <= MAX_EMAIL_ADDRESS_OCTETS &&
    countOctets(getEmailLocalPart(email)) <= MAX_EMAIL_ADDRESS_LOCAL_PART_OCTETS
  );
}

function countOctets(text: string): number {
  return new TextEncoder().encode(text).length;
}

function getEmailLocalPart(email: string): string {
  return email.slice(0, email.lastIndexOf("@"));
}
