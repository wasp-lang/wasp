export type SetupDbResult = {
  waitUntilReady: () => Promise<{
    dbEnvVars: { [envVarName: string]: string };
  }>;
} & AsyncDisposable;
