import fs from 'fs';
import path from 'node:path';
import { $ } from 'zx';

// For some reason, the colors from the chalk package wouldn't

import { Command } from 'commander';
import { ProcessOutput, Shell } from 'zx/core';
import { exit } from 'process';

// show up when run as a subprocess by the Wasp CLI. This works.
export function waspSays(str: string): void {
    console.log('🚀 \x1b[33m ' + str + ' \x1b[0m');
}

export function displayWaspRocketImage(): void {
    // Escaping backslashes makes it look weird here, but it works in console.
    const asciiArt = `

                    __
                   // \\
                   \\\\_/ //
             _    -(||)(')
            \\ \\____///_____
   #########[==__DEPLOYED__}
            /_/

  `;
    console.log(asciiArt);
}

// eslint-disable-next-line
export function makeIdempotent<F extends () => any>(
    fn: F,
): () => ReturnType<F> {
    let result: { value: ReturnType<F> } | null = null;

    return function idempotentFn() {
        if (!result) {
            result = { value: fn() };
        }
        return result.value;
    };
}

export function getCommandHelp(command: Command): string {
    return trimUsage(command.helpInformation());
}

function trimUsage(usage: string): string {
    return usage
        .split(/[\r\n]+/)[0]
        .replace('Usage: ', '')
        .replace(' [options]', '');
}

// There is a theoretical race condition here since we are modifying a global `$`
// property, that when we yield to the `await cmd($)` call that some other calls to
// `$` could use a different verbosity setting. Additionally, calling `silence` multiple
// times concurrently could change the setting incorrectly.
// However, our pattern of awaiting for both `$` and `silence` calls without any random
// callbacks using either means this interleaving should not ever happen.
export async function silence(
    cmd: ($hh: Shell) => Promise<ProcessOutput>,
): Promise<ProcessOutput> {
    const verboseSetting = $.verbose;
    $.verbose = false;
    const proc = await cmd($);
    $.verbose = verboseSetting;
    return proc;
}

export function isYes(str: string): boolean {
    return str.trim().toLowerCase().startsWith('y');
}

export function ensureDirAbsoluteAndExists({
    label,
    dir,
}: {
    label: string;
    dir: string | undefined;
}): void {
    if (!dir) return;

    if (!path.isAbsolute(dir)) {
        waspSays(`The ${label} path must be absolute.`);
        exit(1);
    }

    const dirExists = fs.existsSync(dir);
    if (!dirExists) {
        waspSays(`The ${label} path does not exist.`);
        exit(1);
    }
}
