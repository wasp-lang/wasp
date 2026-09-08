import { WaspCliExe, WaspProjectDir } from "../../common/brandedTypes.js";

export interface CommonCmdOptions {
  waspExe: WaspCliExe;
  waspProjectDir: WaspProjectDir;
  flyTomlDir?: string;
  org?: string;
}

/**
 * All database CLI options are optional for users, for fields we require we provide defaults.
 */
export interface DbOptions {
  dbVmSize: string;
  // These `dbVm*` options override the CPU and memory values set by `dbVmSize`.
  dbVmMemory?: string;
  dbVmCpus?: string;
  dbVmCpuKind?: string;
  dbInitialClusterSize: string;
  dbVolumeSize: string;
  dbImage: string;
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
