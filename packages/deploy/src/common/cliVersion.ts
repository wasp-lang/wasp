import semver, { SemVer } from "semver";

export function parseCliVersion(cliName: string, rawVersion: string): SemVer {
  const version = semver.parse(rawVersion);

  if (version === null) {
    throw new Error(`Unable to parse ${cliName} version "${rawVersion}".`);
  }

  return version;
}

export function assertCliVersionMeetsMinimum({
  cliName,
  currentVersion,
  minimumVersion,
  updateInstructions,
}: {
  cliName: string;
  currentVersion: SemVer;
  minimumVersion: SemVer;
  updateInstructions: string;
}): void {
  if (!semver.gte(currentVersion, minimumVersion)) {
    throw new Error(
      [
        `Wasp expects at least ${cliName} version ${minimumVersion}.`,
        updateInstructions,
      ].join("\n"),
    );
  }
}
