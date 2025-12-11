export interface SubPackageInfo {
  packageName: string;
}

export interface MainPackageData {
  subPackages: {
    [os: string]: {
      [arch: string]: {
        [libcOrUnknown: string]: SubPackageInfo;
      };
    };
  };
}

export interface SubPackageAPI {
  waspBinPath: string;
  dataDirPath: string;
}
