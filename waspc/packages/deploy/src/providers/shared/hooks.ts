import fs from 'fs';
import path from 'node:path';
import { Command } from 'commander';
import { waspSays } from './helpers.js';
import { exit } from 'process';

export function ensureWaspDirLooksRight(thisCommand: Command): void {
    const dirContainsWasproot = fs.existsSync(
        path.join(thisCommand.opts().waspProjectDir, '.wasproot'),
    );
    if (dirContainsWasproot) {
        return;
    }

    waspSays(
        'The supplied Wasp directory does not appear to be a valid Wasp project.',
    );
    waspSays('Please double check your path.');
    exit(1);
}
