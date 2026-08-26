// TODO: If we ever need a more quality email validator, its worth to
// look at https://github.com/JoshData/python-email-validator for inspiration.

/**
 * The syntax we accept is the HTML5 `input[type=email]` grammar,
 * widened to also accept most unicode characters (HTML5 is ASCII only).
 *
 * We widen it to unicode letters, marks and decimal digits, leaving out
 * the possibly dangerous format characters. What still gets through is
 * handled separately later.
 *
 * We start from the HTML5 grammar because it is a good compromise
 * between completeness and complexity.
 *
 * @see {@link https://github.com/whatwg/html/issues/4562 WHATWG international email addresses issue}
 */
const HTML5_EMAIL_WITH_UNICODE_REGEX =
  /^[\p{L}\p{M}\p{Nd}.!#$%&'*+/=?^_`{|}~-]+@[\p{L}\p{Nd}](?:[\p{L}\p{M}\p{Nd}-]{0,61}[\p{L}\p{M}\p{Nd}])?(?:\.[\p{L}\p{Nd}](?:[\p{L}\p{M}\p{Nd}-]{0,61}[\p{L}\p{M}\p{Nd}])?)*$/u;

/**
 * Characters that render as nothing, so that two addresses spelled
 * differently look identical on screen.
 */
const INVISIBLE_CHARACTER_REGEX = /\p{Default_Ignorable_Code_Point}/u;

/**
 * A combining mark at the start has no character of its own to attach to,
 * so it lands on whatever text precedes the address when it is rendered.
 */
const LEADING_COMBINING_MARK_REGEX = /^\p{M}/u;

export function isValidEmail(input: unknown): input is string {
  if (typeof input !== "string") {
    return false;
  }

  return (
    HTML5_EMAIL_WITH_UNICODE_REGEX.test(input) &&
    !INVISIBLE_CHARACTER_REGEX.test(input) &&
    !LEADING_COMBINING_MARK_REGEX.test(input) &&
    isEmailOfValidLength(input)
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

// Not punycoded for simplicity.
function countOctets(text: string): number {
  return new TextEncoder().encode(text).length;
}

function getEmailLocalPart(email: string): string {
  return email.slice(0, email.lastIndexOf("@"));
}
