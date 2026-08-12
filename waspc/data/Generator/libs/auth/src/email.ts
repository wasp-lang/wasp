/**
 * The syntax we accept is the HTML5 `input[type=email]` grammar (WHATWG HTML,
 * "valid e-mail address"), widened to also accept Unicode letters, marks and
 * digits so that internationalized addresses (RFC 6531) are not rejected.
 *
 * We match the HTML5 grammar because that is what browsers already enforce on
 * `input[type=email]`, so the client and the server agree on what an address
 * looks like. We widen it because the HTML5 grammar is deliberately ASCII-only
 * (see whatwg/html#4562), which locks out anyone whose address contains, say,
 * an umlaut.
 *
 * Syntax is all we check. Whether an address can actually receive mail is
 * settled by sending it a verification email, not by a regex.
 */
const validEmailRegex =
  /^[\p{L}\p{M}\p{N}.!#$%&'*+/=?^_`{|}~-]+@[\p{L}\p{N}](?:[\p{L}\p{M}\p{N}-]{0,61}[\p{L}\p{M}\p{N}])?(?:\.[\p{L}\p{N}](?:[\p{L}\p{M}\p{N}-]{0,61}[\p{L}\p{M}\p{N}])?)*$/u;

/**
 * Upper bounds from RFC 5321 (4.5.3.1. Size Limits and Minimums), counted in
 * octets because that is how the RFC counts them.
 */
const maxLocalPartOctets = 64;
const maxAddressOctets = 254;

/**
 * Checks that `input` looks like an email address.
 */
export function isValidEmail(input: unknown): boolean {
  if (typeof input !== "string") {
    return false;
  }

  return (
    validEmailRegex.test(input) &&
    countOctets(input) <= maxAddressOctets &&
    countOctets(getLocalPart(input)) <= maxLocalPartOctets
  );
}

function getLocalPart(email: string): string {
  return email.slice(0, email.lastIndexOf("@"));
}

function countOctets(text: string): number {
  return new TextEncoder().encode(text).length;
}
