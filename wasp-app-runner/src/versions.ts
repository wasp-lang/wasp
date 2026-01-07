import * as semver from "semver";
import { WaspVersion } from "./waspCli.js";

export interface VersionSettings {
  clientAppDir: string;
  serverAppDir: string;
}

export const getSettingsForVersion = (
  version: WaspVersion,
): VersionSettings => {
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
