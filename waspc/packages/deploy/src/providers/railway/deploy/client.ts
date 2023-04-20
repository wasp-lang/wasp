import { $ } from 'zx';
import fs from 'fs-extra';
import { CommonOptions } from '../../shared/CommonOptions';
import {
    cdToClientBuildDir,
    displayWaspRocketImage,
    waspSays,
} from '../../shared/helpers.js';
import {
    NGINX_CONFIG_TEMPLATE,
    REACT_DOCKER_TEMPLATE,
} from '../helpers/consts.js';
import { RailwayDeploymentConfig } from '../types';

// !!!!!!!!!!!!!!!!!
// TODO: this is a hack, we should not be doing this, but I need to for now
const HACK_PACKAGES_JSON = () => {
    const packagesJson = fs.readJsonSync('package.json');
    packagesJson.scripts = {
        ...packagesJson.scripts,
        build: 'npm run validate-env && vite build',
    };
    fs.writeJsonSync('package.json', packagesJson, { spaces: 2 });
};

type RwDeploymentInfo = {
    commonOptions: CommonOptions;
    clientService: RailwayDeploymentConfig['clientService'];
};
export async function deployClient({
    commonOptions,
    clientService,
}: RwDeploymentInfo) {
    waspSays('Deploying your client now...');

    cdToClientBuildDir(commonOptions.waspProjectDir);

    waspSays('Building web client for production...');

	HACK_PACKAGES_JSON();

    // TODO: make it skipable, don't need to install deps for re-deployments as it's installed in docker
    await $`npm install`;

    fs.ensureDirSync('.nginx');
    fs.writeFileSync('.nginx/nginx.conf', NGINX_CONFIG_TEMPLATE);

    // Creates the necessary Dockerfile for deploying static websites to Railway.
    const dockerfileContents = REACT_DOCKER_TEMPLATE;
    fs.writeFileSync('Dockerfile', dockerfileContents);
    fs.writeFileSync('.dockerignore', 'node_modules');

    // this command is a stream of logs from server
    // but this implementation does not care about it - fire & forget
    await $`railway up --service ${clientService.name}`;

    displayWaspRocketImage();
    waspSays(
        `Client has been deployed! Your Wasp app is accessible at: ${clientService.url}`,
    );
}
