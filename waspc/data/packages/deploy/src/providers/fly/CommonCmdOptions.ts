import { WaspCliExe, WaspProjectDir } from "../../common/brandedTypes.js";

export interface CommonCmdOptions {
  waspExe: WaspCliExe;
  waspProjectDir: WaspProjectDir;
  flyTomlDir?: string;
  org?: string;
}

export interface DbOptions {
  vmSize: string;
  initialClusterSize: string;
  volumeSize: string;
  dbImage?: string;
}

export interface LocalBuildOptions {
  buildLocally: boolean;
}

export interface SecretsOptions {
  serverSecret: string[];
  clientSecret: string[];
}
