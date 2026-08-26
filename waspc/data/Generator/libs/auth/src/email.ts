// TODO: If we ever need a more quality email validator, its worth to
// look at https://github.com/JoshData/python-email-validator for inspiration.

/**
 * The syntax we accept is the HTML5 `input[type=email]` grammar,
 * widened to also accept Unicode characters.
 *
 * We start from the HTML5 grammar because it is a good compromise between
 * completeness and complexity. We widen it because it is deliberately
 * ASCII-only, which locks out anyone whose address contains unicode.
 *
 * We also do some extra safefty checks by filtering out invisible,
 * text-reordering, zero-width and bidirectional override (LTR or RTL)
 * unicode characters. Also a combining mark cannot start an email address,
 * because it would attach to whatever text that precedes the address.
 *
 * @see {@link https://github.com/whatwg/html/issues/4562 WHATWG international email addresses issue}
 */
const HTML5_EMAIL_WITH_UNICODE_REGEX =
  /^(?!\p{M})[\p{L}\p{M}\p{N}.!#$%&'*+/=?^_`{|}~-]+@[\p{L}\p{N}](?:[\p{L}\p{M}\p{N}-]{0,61}[\p{L}\p{M}\p{N}])?(?:\.[\p{L}\p{N}](?:[\p{L}\p{M}\p{N}-]{0,61}[\p{L}\p{M}\p{N}])?)*$/u;

export function isValidEmail(input: string): boolean {
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
