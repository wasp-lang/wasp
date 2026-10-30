import { $, } from "zx";
export function createCommand(command, defaultOptions = {}) {
    return (commandArgs, options) => {
        return $({ ...defaultOptions, ...options }) `${command} ${commandArgs}`;
    };
}
export function createCommandWithCwd(command, cwd) {
    return createCommand(command, { cwd });
}
export async function runJsonCommand(command, commandArgs, schema) {
    const result = await command(commandArgs, { verbose: false });
    return schema.parse(parseJsonFromStdout(result));
}
export async function tryRunJsonCommand(command, commandArgs, schema) {
    const result = await command(commandArgs, { verbose: false, nothrow: true });
    if (result.exitCode !== 0) {
        return null;
    }
    return schema.parse(parseJsonFromStdout(result));
}
// zx's `ProcessOutput.json()` parses the combined stdout and stderr, but CLIs
// write progress messages to stderr (e.g. `railway add --json` echoes the
// prompt answers there), which would corrupt the JSON.
// Reported to zx: https://github.com/google/zx/issues/1505
function parseJsonFromStdout(output) {
    return JSON.parse(output.stdout);
}
//# sourceMappingURL=zx.js.map