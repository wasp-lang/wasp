// TODO: If we ever need a more quality email validator, its worth to
// look at https://github.com/JoshData/python-email-validator for inspiration.
/**
 * The syntax we accept is the HTML5 `input[type=email]` grammar, widened to
 * also accept Unicode letters, marks and digits so that internationalized
 * addresses are not rejected.
 *
 * We start from the HTML5 grammar rather than RFC 5322 because it is a good
 * compromise. It is a willful violation of the RFC that drops the corners
 * nobody uses (quoted local parts, comments, IP-literal domains) and keeps
 * what remains simple enough to read as a single regex. We widen it because
 * it is deliberately ASCII-only, which locks out anyone whose address
 * contains unicode.
 * @see https://github.com/whatwg/html/issues/4562
 *
 * Letters, marks and digits are stricter than RFC 6531, which allows any
 * non-ASCII character in the local part. We leave out the invisible and
 * text-reordering ones, like zero-width joiners and bidirectional overrides,
 * because they can make two different addresses look identical on screen. A
 * combining mark cannot start the local part for the same reason: with no
 * character to attach to, it lands on whatever text precedes the address.
 */
const HTML5_UNICODE_EMAIL_REGEX =
  /^(?!\p{M})[\p{L}\p{M}\p{N}.!#$%&'*+/=?^_`{|}~-]+@[\p{L}\p{N}](?:[\p{L}\p{M}\p{N}-]{0,61}[\p{L}\p{M}\p{N}])?(?:\.[\p{L}\p{N}](?:[\p{L}\p{M}\p{N}-]{0,61}[\p{L}\p{M}\p{N}])?)*$/u;

/**
 * Upper bounds from RFC 5321 (4.5.3.1. Size Limits and Minimums).
 */
const MAX_LOCAL_PART_OCTESTS = 64;
const MAX_ADDRESS_OCTETS = 254;

export function isValidEmail(input: unknown): boolean {
  if (typeof input !== "string") {
    return false;
  }

  return (
    HTML5_UNICODE_EMAIL_REGEX.test(input) &&
    countOctets(input) <= MAX_ADDRESS_OCTETS &&
    countOctets(getEmailLocalPart(input)) <= MAX_LOCAL_PART_OCTESTS
  );
}

function getEmailLocalPart(email: string): string {
  return email.slice(0, email.lastIndexOf("@"));
}

function countOctets(text: string): number {
  return new TextEncoder().encode(text).length;
}
