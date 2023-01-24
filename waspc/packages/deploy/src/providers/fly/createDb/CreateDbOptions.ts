import { IGlobalOptions } from '../GlobalOptions.js'

export interface ICreateDbOptions extends IGlobalOptions {
  vmSize: string
  initialClusterSize: string
  volumeSize: string
}
