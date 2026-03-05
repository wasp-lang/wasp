export interface SubPackageInfo {
  packageName: string;
}

// This can be any string representing a lack of a defined libc for the build
// (e.g. macOS or Windows don't use libc in the same way Linux does).
// The usual convention is to call it "unknown".
export const UNDEFINED_LIBC_NAME = "unknown" as const;

export interface MainPackageData {
  subPackages: {
    [os: string]: {
      [arch: string]: {
        [libc: string | typeof UNDEFINED_LIBC_NAME]: SubPackageInfo;
      };
    };
  };
}

export interface SubPackageAPI {
  waspBinPath: string;
  dataDirPath: string;
}
