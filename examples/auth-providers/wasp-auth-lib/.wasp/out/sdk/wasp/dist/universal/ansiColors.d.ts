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
export declare function colorize(color: keyof typeof ansiColorCodes, text: string): string;
declare const ansiColorCodes: {
    readonly red: "\u001B[31m";
    readonly yellow: "\u001B[33m";
};
export {};
//# sourceMappingURL=ansiColors.d.ts.map