export interface CommonOptions {
  waspExe: string;
  railwayExe: string;
  waspProjectDir: string;
  skipBuild?: boolean;
}

export interface SecretsOptions {
  serverSecret: string[];
  clientSecret: string[];
}
