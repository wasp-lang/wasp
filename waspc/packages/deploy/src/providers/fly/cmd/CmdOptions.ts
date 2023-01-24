import { IGlobalOptions } from '../GlobalOptions.js'

export interface ICmdOptions extends IGlobalOptions {
  context: string
}

export const SERVER_CONTEXT_OPTION = 'server'
export const CLIENT_CONTEXT_OPTION = 'client'
