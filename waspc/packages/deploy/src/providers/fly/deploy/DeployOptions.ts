import { IGlobalOptions } from '../GlobalOptions.js'

export interface IDeployOptions extends IGlobalOptions {
  skipBuild?: boolean
}
