export function createDeploymentInstructions({ baseName, cmdOptions, tomlFilePaths, region, }) {
    return Object.freeze({
        baseName,
        region,
        cmdOptions,
        tomlFilePaths,
        clientFlyAppName: `${baseName}-client`,
        serverFlyAppName: `${baseName}-server`,
        dbName: `${baseName}-db`,
    });
}
//# sourceMappingURL=DeploymentInstructions.js.map