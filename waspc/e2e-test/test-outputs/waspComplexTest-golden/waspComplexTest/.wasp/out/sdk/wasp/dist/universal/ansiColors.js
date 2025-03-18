export const colors = {
    red: '\x1b[31m',
    yellow: '\x1b[33m',
};
export const resetColor = "\x1b[0m";
// PRIVATE API (SDK, client)
// Used with console.log() to colorize the output
// Example: console.log(getColorizedConsoleFormatString('red'), 'This is red text');
export function getColorizedConsoleFormatString(colorKey) {
    const color = colors[colorKey];
    return `${color}%s${resetColor}`;
}
//# sourceMappingURL=ansiColors.js.map