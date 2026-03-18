// PRIVATE API (SDK, client)
/**
 * Wraps each line of text with ANSI color codes.
 * Only works in Node.js (server-side), not in the browser.
 *
 * Each line is individually wrapped because Wasp reads child process
 * output line-by-line and re-prints it with a prefix (e.g. `[ Server ]`).
 * A single color code spanning multiple lines would only color the first line.
 *
 * @example
 * ```typescript
 * console.log(colorize('red', 'This is red text'));
 * ```
 *
 * @internal This is a private API for: SDK, client.
 */
export function colorize(color: keyof typeof ansiColorCodes, text: string): string {
  const ansiColorCode = ansiColorCodes[color];
  return text
    .split("\n")
    .map((line) => `${ansiColorCode}${line}${ansiResetCode}`)
    .join("\n");
}

const ansiColorCodes = {
  red: "\x1b[31m",
  yellow: "\x1b[33m",
} as const;

const ansiResetCode = "\x1b[0m";
