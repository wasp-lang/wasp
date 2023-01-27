import { GlobalOptions } from '../GlobalOptions.js';

export interface CreateDbOptions extends GlobalOptions {
  vmSize: string;
  initialClusterSize: string;
  volumeSize: string;
}
