export function mergeDefaultAndUserConfig<DefaultConfig extends object, UserConfig extends object>(
  defaultConfig: DefaultConfig,
  userConfigFn: () => UserConfig,
): DefaultConfig & UserConfig;
export function mergeDefaultAndUserConfig<DefaultConfig extends object, UserConfig extends object>(
  defaultConfig: DefaultConfig
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
