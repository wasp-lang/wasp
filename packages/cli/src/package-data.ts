// The shape of `data.json`, which `scripts/make-cli-npm-packages` writes into
// the published `@wasp/cli`, and of the platform sub-packages it lists.

export interface SubPackageInfo {
  packageName: string;
}

// This can be any string representing a lack of a defined libc for the build
// (e.g. macOS or Windows don't use libc in the same way Linux does).
// The usual convention is to call it "unknown".
export const UNDEFINED_LIBC_NAME = "unknown";

export interface CliPackageData {
  subPackages: {
    [os: string]: {
      [arch: string]: {
        [libc: string]: SubPackageInfo;
      };
    };
  };
}

export interface SubPackageAPI {
  waspBinPath: string;
  dataDirPath: string;
}
