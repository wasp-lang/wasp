{{={= =}=}}
const _config = new Map<string, Record<string, unknown>>();
{=# moduleProvides =}
_config.set({=& packageNameJson =}, {=& valuesJson =});
{=/ moduleProvides =}
export const config = _config;
