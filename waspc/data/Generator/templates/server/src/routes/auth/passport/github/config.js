import { configFn } from './configMapping.js'
export { getUserFieldsFn } from './configMapping.js'

// Validates the provided config function returns all required data.
export const config = ((config) => {
  if (!config?.clientID) {
    throw new Error("The GitHub configFn must return an object with a clientID property.")
  }

  if (!config?.clientSecret) {
    throw new Error("The GitHub configFn must return an object with a clientSecret property.")
  }

  if (!config?.scope || !Array.isArray(config.scope)) {
    throw new Error("The GitHub configFn must return an object with a scope property.")
  }

  return config
})(await configFn())
