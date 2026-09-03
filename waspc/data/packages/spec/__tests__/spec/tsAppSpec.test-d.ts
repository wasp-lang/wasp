// We are only interested in testing the types, so we don't actually need to use
// the variables we define here:
/* eslint-disable @typescript-eslint/no-unused-vars */

import { describe, expectTypeOf, test } from "vitest";
import { customAuthProvider } from "../../src/spec/publicApi/constructors.js";
import type * as WaspSpec from "../../src/spec/publicApi/waspSpec.js";

describe("Auth providers", () => {
  const provider = customAuthProvider({
    id: "test",
    server: { from: "./src/auth", import: "provider" } as never,
  });

  test("accepts a manifest built by a spec helper", () => {
    expectTypeOf<typeof provider>().toExtend<WaspSpec.AuthProviderConfig>();
  });

  // The two tests below pin excess-property checking on literals: writing a
  // provider-package field directly on `auth` must be flagged at the site
  // where users actually write it (an object literal), which
  // `@ts-expect-error` asserts. Plain assignability cannot catch extra
  // properties.
  test("forbids the old flat shape: methods on auth itself", () => {
    const _invalid: WaspSpec.Auth = {
      userEntity: "User",
      onAuthFailedRedirectTo: "/login",
      providers: [provider],
      // @ts-expect-error -- methods belong to the auth package's spec helper
      methods: { usernameAndPassword: {} },
    };
  });

  test("forbids bare hook fields on auth itself (they live under auth.hooks)", () => {
    const _invalid: WaspSpec.Auth = {
      userEntity: "User",
      onAuthFailedRedirectTo: "/login",
      providers: [provider],
      // @ts-expect-error -- lifecycle hooks live under auth.hooks
      onBeforeSignup: () => undefined,
    };
  });

  test("forbids a manifest without the authenticity marker", () => {
    expectTypeOf<{
      kind: "external";
      contractVersion: 1;
      id: "clerk";
      server: { package: "@wasp.sh/auth-clerk/server" };
      capabilities: string[];
      env: { server: []; client: [] };
    }>().not.toExtend<WaspSpec.AuthProviderConfig>();
  });
});
