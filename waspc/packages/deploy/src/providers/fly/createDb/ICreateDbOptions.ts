import { IGlobalOptions } from '../IGlobalOptions.js'

export interface ICreateDbOptions extends IGlobalOptions {
  vmSize: string
  initialClusterSize: string
  volumeSize: string
}
