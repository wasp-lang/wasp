import { configFn } from './configMapping.js'
export { getUserFieldsFn } from './configMapping.js'

// Validates the provided config function returns all required data.
export const config = ((config) => {
  if (!config?.clientID) {
    throw new Error("The Google configFn must return an object with a clientID property.")
  }

  if (!config?.clientSecret) {
    throw new Error("The Google configFn must return an object with a clientSecret property.")
  }

  if (!config?.scope) {
    throw new Error("The Google configFn must return an object with a scope property.")
  } else if (!Array.isArray(config.scope) || !config.scope.includes('profile')) {
    throw new Error("The Google configFn returned an object with an invalid scope property. It must be an array including 'profile'.")
  }

  return config
})(await configFn())
