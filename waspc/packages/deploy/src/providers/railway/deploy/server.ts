import { $ } from 'zx';
import {
    cdToServerBuildDir,
    displayWaspRocketImage,
    waspSays,
} from '../../shared/helpers.js';
import { RwServerDeploymentInfo } from './RwDeploymentInfo';

export async function deployServer({
    commonOptions,
    serverService,
}: RwServerDeploymentInfo) {
    waspSays('Deploying your server now...');
    cdToServerBuildDir(commonOptions.waspProjectDir);

    // this command is a stream of logs from server
    // but this implementation does not care about it - fire & forget
    await $`railway up --service ${serverService.name}`;

    displayWaspRocketImage();
    waspSays(
        `Server has been deployed! Your Wasp app is accessible at: ${serverService.url}`,
    );
}
