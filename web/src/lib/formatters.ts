// All of our docs are written in US English, so our formatters should also use it.
export const LOCALE = "en-US";

/**
 * Formats an array of strings into a human-readable list,
 * separated by commas and an "and".
 *
 * @example
 * ```
 * In US English:
 * ["foo"] -> "foo"
 * ["foo", "bar"] -> "foo and bar"
 * ["foo", "bar", "baz"] -> "foo, bar, and baz"
 * ["foo", "bar", "baz", "qux"] -> "foo, bar, baz, and qux"
 * ```
 */
export const listFormatter = new Intl.ListFormat(LOCALE, {
  type: "conjunction",
});

/**
 * Formats a `Date` into a human-readable string.
 *
 * @example
 * ```
 * In US English:
 * new Date("2023-01-15") -> "Jan 15, 2023"
 * new Date("2024-12-05") -> "Dec 5, 2024"
 * ```
 */
export const dateFormatter = new Intl.DateTimeFormat(LOCALE, {
  year: "numeric",
  month: "short",
  day: "numeric",
  timeZone: "UTC", // Ensure consistent date formatting regardless of user's timezone
});
