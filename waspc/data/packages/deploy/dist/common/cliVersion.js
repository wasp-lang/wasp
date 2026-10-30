import semver from "semver";
export function parseCliVersion(cliName, rawVersion) {
    const version = semver.parse(rawVersion);
    if (version === null) {
        throw new Error(`Unable to parse ${cliName} version "${rawVersion}".`);
    }
    return version;
}
export function assertCliVersionMeetsMinimum({ cliName, currentVersion, minimumVersion, updateInstructions, }) {
    if (!semver.gte(currentVersion, minimumVersion)) {
        throw new Error([
            `Wasp expects at least ${cliName} version ${minimumVersion}.`,
            updateInstructions,
        ].join("\n"));
    }
}
//# sourceMappingURL=cliVersion.js.map