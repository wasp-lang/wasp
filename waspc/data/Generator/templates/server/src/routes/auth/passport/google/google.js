import { configFn } from './googleDefaults.js' // TODO: template
export { getUserFieldsFn } from './googleDefaults.js' // TODO: template

// Validates the provided config function returns all required data.
export const config = ((config) => {
  if (!config?.clientId) {
    throw new Error("auth.google.configFn must return an object with a clientId property.")
  }

  if (!config?.clientSecret) {
    throw new Error("auth.google.configFn must return an object with a clientSecret property.")
  }

  if (!config?.scope) {
    throw new Error("auth.google.configFn must return an object with a scope property.")
  } else if (!Array.isArray(config.scope) || !config.scope.includes('profile')) {
    throw new Error("auth.google.configFn returned an object with an invalid scope property. It must be an array including 'profile'.")
  }

  return config
})(await configFn())
