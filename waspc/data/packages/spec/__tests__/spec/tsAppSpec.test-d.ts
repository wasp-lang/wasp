// We are only interested in testing the types, so we don't actually need to use
// the variables we define here:
/* eslint-disable @typescript-eslint/no-unused-vars */

import { describe, expectTypeOf, test } from "vitest";
import { customAuthHandler } from "../../src/spec/publicApi/constructors.js";
import type * as WaspSpec from "../../src/spec/publicApi/waspSpec.js";

describe("Auth schemes", () => {
  const scheme = customAuthHandler({
    server: { from: "./src/auth", import: "handler" } as never,
  });

  test("accepts a manifest built by a spec helper", () => {
    expectTypeOf<typeof scheme>().toExtend<WaspSpec.AuthSchemeManifest>();
  });

  // The two tests below pin excess-property checking on literals: writing a
  // handler-package field directly on `auth` must be flagged at the site
  // where users actually write it (an object literal), which
  // `@ts-expect-error` asserts. Plain assignability cannot catch extra
  // properties.
  test("forbids the old flat shape: methods on auth itself", () => {
    const _invalid: WaspSpec.Auth = {
      userEntity: "User",
      onAuthFailedRedirectTo: "/login",
      schemes: { test: scheme },
      // @ts-expect-error -- methods belong to the auth package's spec helper
      methods: { usernameAndPassword: {} },
    };
  });

  test("forbids bare hook fields on auth itself (they live under auth.hooks)", () => {
    const _invalid: WaspSpec.Auth = {
      userEntity: "User",
      onAuthFailedRedirectTo: "/login",
      schemes: { test: scheme },
      // @ts-expect-error -- lifecycle hooks live under auth.hooks
      onBeforeSignup: () => undefined,
    };
  });

  test("forbids a manifest without the authenticity marker", () => {
    expectTypeOf<{
      kind: "scheme";
      contractVersion: 12;
      server: {
        authAdapter: { package: "@wasp.sh/auth-clerk/server" };
      };
      capabilities: string[];
    }>().not.toExtend<WaspSpec.AuthSchemeManifest>();
  });
});
