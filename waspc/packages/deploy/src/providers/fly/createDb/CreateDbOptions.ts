import { CommonOptions } from '../CommonOptions.js';

export interface CreateDbOptions extends CommonOptions {
  vmSize: string;
  initialClusterSize: string;
  volumeSize: string;
}
