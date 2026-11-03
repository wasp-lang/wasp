import {
  CommonCmdOptions,
  CustomServerUrlOption,
  DbOptions,
  LocalBuildOptions,
  SecretsOptions,
} from "../../CommonCmdOptions.js";

export interface LaunchCmdOptions
  extends CommonCmdOptions,
    DbOptions,
    LocalBuildOptions,
    SecretsOptions,
    CustomServerUrlOption {}
