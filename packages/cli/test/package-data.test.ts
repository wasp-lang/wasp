import { describe, expect, test } from "vitest";
import type { CliPackageData } from "wasp-cli-package-data-types";
import {
  findSubPackageInfo,
  UNDEFINED_LIBC_NAME,
} from "../src/package-data.js";

const packageData: CliPackageData = {
  subPackages: {
    darwin: {
      arm64: { [UNDEFINED_LIBC_NAME]: { packageName: "wasp-darwin-arm64" } },
    },
    linux: {
      x64: {
        glibc: { packageName: "wasp-linux-x64-glibc" },
        musl: { packageName: "wasp-linux-x64-musl" },
      },
    },
  },
};

describe("findSubPackageInfo", () => {
  test("finds the sub-package for the platform's libc", () => {
    expect(
      findSubPackageInfo(packageData, {
        os: "linux",
        cpu: "x64",
        libc: "musl",
      }),
    ).toEqual({ packageName: "wasp-linux-x64-musl" });
  });

  test("finds the sub-package of a platform where libc doesn't apply", () => {
    expect(
      findSubPackageInfo(packageData, {
        os: "darwin",
        cpu: "arm64",
        libc: UNDEFINED_LIBC_NAME,
      }),
    ).toEqual({ packageName: "wasp-darwin-arm64" });
  });

  test("finds nothing for a platform without a sub-package", () => {
    expect(
      findSubPackageInfo(packageData, {
        os: "win32",
        cpu: "x64",
        libc: UNDEFINED_LIBC_NAME,
      }),
    ).toBeUndefined();
  });
});
