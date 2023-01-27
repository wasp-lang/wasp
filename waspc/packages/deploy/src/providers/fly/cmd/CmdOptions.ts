import { GlobalOptions } from '../GlobalOptions.js';

export interface CmdOptions extends GlobalOptions {
  context: string;
}

export const SERVER_CONTEXT_OPTION = 'server';

export const CLIENT_CONTEXT_OPTION = 'client';
