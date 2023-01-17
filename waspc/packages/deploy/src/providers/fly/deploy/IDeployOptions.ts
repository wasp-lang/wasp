import { IGlobalOptions } from '../IGlobalOptions.js'

export interface IDeployOptions extends IGlobalOptions {
  skipBuild?: boolean
}
