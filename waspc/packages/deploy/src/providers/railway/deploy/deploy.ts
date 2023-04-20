import { $, cd } from 'zx';
import { makeIdempotent, waspSays } from '../../shared/helpers.js';
import { RwDeployOptions } from './DeployOptions';

export async function deploy(options: RwDeployOptions): Promise<void> {
    waspSays('Deploying your Wasp app to Fly.io!');

    const buildWasp = makeIdempotent(async () => {
        if (options.skipBuild) {
            return;
        }

        waspSays('Building your Wasp app...');
        cd(options.waspProjectDir);
        await $`${options.waspExe} build`;
    });

    /* const tomlFilePaths = getTomlFilePaths(options); */
    /**/
    /* // NOTE: Below, it would be nice if we could store the client, server, and DB names somewhere. */
    /* // For now we just rely on the suffix naming convention and infer from toml files. */
    /* if (!serverTomlExistsInProject(tomlFilePaths)) { */
    /*     waspSays( */
    /*         `${ */
    /*             tomlFilePaths.serverTomlPath */
    /*         } missing. Skipping server deploy. Perhaps you need to run "${getCommandHelp( */
    /*             flySetupCommand, */
    /*         )}" first?`, */
    /*     ); */
    /* } else if (options.skipServer) { */
    /*     waspSays('Skipping server deploy due to CLI option.'); */
    /* } else { */
    /*     const inferredBaseName = getInferredBasenameFromServerToml( */
    /*         tomlFilePaths, */
    /*     ); */
    /*     const deploymentInfo = createDeploymentInfo( */
    /*         inferredBaseName, */
    /*         undefined, */
    /*         options, */
    /*         tomlFilePaths, */
    /*     ); */
    /*     await buildWasp(); */
    /*     await deployServer(deploymentInfo, options); */
    /* } */
    /**/
    /* if (!clientTomlExistsInProject(tomlFilePaths)) { */
    /*     waspSays( */
    /*         `${ */
    /*             tomlFilePaths.clientTomlPath */
    /*         } missing. Skipping client deploy. Perhaps you need to run "${getCommandHelp( */
    /*             flySetupCommand, */
    /*         )}" first?`, */
    /*     ); */
    /* } else if (options.skipClient) { */
    /*     waspSays('Skipping client deploy due to CLI option.'); */
    /* } else { */
    /*     const inferredBaseName = getInferredBasenameFromClientToml( */
    /*         tomlFilePaths, */
    /*     ); */
    /*     const deploymentInfo = createDeploymentInfo( */
    /*         inferredBaseName, */
    /*         undefined, */
    /*         options, */
    /*         tomlFilePaths, */
    /*     ); */
    /*     await buildWasp(); */
    /*     await deployClient(deploymentInfo, options); */
    /* } */
}
