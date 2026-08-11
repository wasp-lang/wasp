export function mergeDefaultAndUserConfig<DefaultConfig extends object, UserConfig extends object>(
  defaultConfig: DefaultConfig,
  userConfigFn: () => UserConfig,
): DefaultConfig & UserConfig;
export function mergeDefaultAndUserConfig<DefaultConfig extends object>(
  defaultConfig: DefaultConfig,
  userConfigFn?: undefined,
): DefaultConfig;
export function mergeDefaultAndUserConfig<DefaultConfig extends object, UserConfig extends object>(
  defaultConfig: DefaultConfig,
  userConfigFn?: () => UserConfig,
): DefaultConfig {
    if (!userConfigFn) {
        return defaultConfig;
    }
    return {
      ...defaultConfig,
      ...userConfigFn(),
    }
}
