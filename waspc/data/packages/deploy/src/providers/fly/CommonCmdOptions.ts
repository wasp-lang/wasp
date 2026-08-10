import { WaspCliExe, WaspProjectDir } from "../../common/brandedTypes.js";

export interface CommonCmdOptions {
  waspExe: WaspCliExe;
  waspProjectDir: WaspProjectDir;
  flyTomlDir?: string;
  org?: string;
}

export interface DbOptions {
  dbVmSize: string;
  dbVmMemory?: string;
  dbVmCpus?: string;
  dbVmCpuKind?: string;
  dbInitialClusterSize: string;
  dbVolumeSize: string;
  dbImage?: string;
}

export interface LocalBuildOptions {
  buildLocally: boolean;
}

export interface SecretsOptions {
  serverSecret: string[];
  clientSecret: string[];
}

export interface CustomServerUrlOption {
  customServerUrl?: string;
}
