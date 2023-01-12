import { IGlobalOptions } from '../IGlobalOptions.js'

export interface ICmdOptions extends IGlobalOptions {
  context: string
}

export const SERVER_CONTEXT_OPTION = 'server'
export const CLIENT_CONTEXT_OPTION = 'client'
