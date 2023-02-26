import { configFn } from './configMapping.js'
export { getUserFieldsFn } from './configMapping.js'

export const config = ((config) => {
  return config
})(await configFn())
