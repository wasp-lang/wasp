export type UserDefinedConfigFn = () => { [key: string]: any };

export function mergeDefaultAndUserConfig<
  DefaultConfig extends object,
  UserConfig extends object,
>(
  defaultConfig: DefaultConfig,
  userConfigFn: () => UserConfig,
): DefaultConfig & UserConfig;
export function mergeDefaultAndUserConfig<
  DefaultConfig extends object,
  UserConfig extends object,
>(
  defaultConfig: DefaultConfig,
  userConfigFn?: (() => UserConfig) | undefined,
): DefaultConfig;
export function mergeDefaultAndUserConfig<
  DefaultConfig extends object,
  UserConfig extends object,
>(
  defaultConfig: DefaultConfig,
  userConfigFn?: (() => UserConfig) | undefined,
): DefaultConfig {
  if (!userConfigFn) {
    return defaultConfig;
  }
  return {
    ...defaultConfig,
    ...userConfigFn(),
  };
}
