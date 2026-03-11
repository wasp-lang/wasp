export interface SetupDbResult extends Disposable {
  dbEnvVars: { [envVarName: string]: string };
}
