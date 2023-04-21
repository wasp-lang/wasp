import { $, cd } from 'zx';
import { makeIdempotent, waspSays } from '../../shared/helpers.js';
import { RAILWAY_SERVICE_NAME } from '../helpers/consts.js';
import { ensureProjectLinked, getRailwayConfig } from '../helpers/helpers.js';
import { deployClient } from './client.js';
import { deployServer } from './server.js';

import type { RwDeployOptions } from './RwDeployOptions';

export async function deploy(options: RwDeployOptions): Promise<void> {
    waspSays(`Deploying your Wasp app to ${RAILWAY_SERVICE_NAME}!`);
    cd(options.waspProjectDir);

    const rwConfig = getRailwayConfig();
    await ensureProjectLinked({
        environment: rwConfig.environment,
        projectId: rwConfig.projectId,
    });

    const buildWasp = makeIdempotent(async () => {
        if (options.skipBuild) {
            return;
        }

        waspSays('Building your Wasp app...');
        await $`${options.waspExe} build`;
    });

    if (!options.skipBuild) {
        await buildWasp();
    }

    if (!options.skipServer) {
        await deployServer({
            commonOptions: options,
            serverService: rwConfig.serverService,
        });
    }

    if (!options.skipClient) {
        await deployClient({
            commonOptions: options,
            clientService: rwConfig.clientService,
        });
    }
}
