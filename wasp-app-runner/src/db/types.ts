export type SetupDbFn = (options: {
  appName: string;
  pathToApp: string;
}) => Promise<{
  dbEnvVars: { [envVarName: string]: string };
}>;
