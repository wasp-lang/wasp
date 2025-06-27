import { WaspCliExe, WaspProjectDir } from "../../common/brandedTypes.js";

export interface CommonOptions {
  waspExe: WaspCliExe;
  waspProjectDir: WaspProjectDir;
  flyTomlDir?: string;
  org?: string;
}

export interface DbOptions {
  vmSize: string;
  initialClusterSize: string;
  volumeSize: string;
}

export interface LocalBuildOptions {
  buildLocally: boolean;
}

export interface SecretsOptions {
  serverSecret: string[];
  clientSecret: string[];
}
