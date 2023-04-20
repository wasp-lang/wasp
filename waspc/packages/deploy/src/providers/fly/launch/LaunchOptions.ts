import {
    CommonOptions,
    DbOptions,
    LocalBuildOptions,
} from '../../shared/CommonOptions';

export interface LaunchOptions
    extends CommonOptions,
        DbOptions,
        LocalBuildOptions {}
