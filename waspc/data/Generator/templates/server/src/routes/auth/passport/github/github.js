import { configFn } from './githubDefaults.js' // TODO: template
export { getUserFieldsFn } from './githubDefaults.js' // TODO: template

// Validates the provided config function returns all required data.
export const config = ((config) => {
  if (!config?.clientId) {
    throw new Error("auth.github.configFn must return an object with a clientId property.")
  }

  if (!config?.clientSecret) {
    throw new Error("auth.github.configFn must return an object with a clientSecret property.")
  }

  if (!config?.scope || !Array.isArray(config.scope)) {
    throw new Error("auth.github.configFn must return an object with a scope property.")
  }

  return config
})(await configFn())
