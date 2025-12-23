import * as semver from "semver";

export interface VersionSettings {
  clientAppDir: string;
  serverAppDir: string;
}

export const getSettingsForVersion = (version: string): VersionSettings => {
  if (semver.lt(version, "0.21.0")) {
    return {
      clientAppDir: ".wasp/build/web-app",
      serverAppDir: ".wasp/build",
    };
  } else {
    return {
      clientAppDir: ".wasp/out/web-app",
      serverAppDir: ".wasp/out",
    };
  }
};
