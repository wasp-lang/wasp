// The format of `data.json`, which lists the sub-packages that hold the Wasp
// binary (one per platform), and the API each sub-package exports.

export interface CliSubPackageInfo {
  packageName: string;
}

export interface CliPackageData {
  subPackages: {
    [os: string]: {
      [arch: string]: {
        // `UNDEFINED_LIBC_NAME` of `@wasp/cli` where libc doesn't apply.
        [libc: string]: CliSubPackageInfo;
      };
    };
  };
}

export interface CliSubPackageAPI {
  waspBinPath: string;
  dataDirPath: string;
}

export interface PlatformTarget {
  os: string;
  cpu: string;
  libc: string;
}
