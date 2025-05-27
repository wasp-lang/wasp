import escapeStringRegexp from "escape-string-regexp";

export function wrapInWordBoundaries(value: string): RegExp {
  const escapedValue = escapeStringRegexp(value);
  const regexStr = String.raw`\b${escapedValue}\b`;
  return new RegExp(regexStr);
}
