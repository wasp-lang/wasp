// We are only interested in testing the types, so we don't actually need to use
// the variables we define here:
/* eslint-disable @typescript-eslint/no-unused-vars */

import { describe, expectTypeOf, test } from "vitest";
import { waspAuth } from "../../src/spec/publicApi/constructors.js";
import type * as WaspSpec from "../../src/spec/publicApi/waspSpec.js";

describe("AuthMethods", () => {
  const usernameAndPassword: Required<
    Pick<WaspSpec.AuthMethods, "usernameAndPassword">
  > = {
    usernameAndPassword: {},
  };

  const email: Required<Pick<WaspSpec.AuthMethods, "email">> = {
    email: {
      fromField: { email: "noreply@example.com" },
      emailVerification: { clientRoute: "/verify" },
      passwordReset: { clientRoute: "/reset" },
    },
  };

  const google: Required<Pick<WaspSpec.AuthMethods, "google">> = {
    google: {},
  };

  const slack: Required<Pick<WaspSpec.AuthMethods, "slack">> = {
    slack: {},
  };

  test("allows only usernameAndPassword", () => {
    expectTypeOf<typeof usernameAndPassword>().toExtend<WaspSpec.AuthMethods>();
  });

  test("allows only email", () => {
    expectTypeOf<typeof email>().toExtend<WaspSpec.AuthMethods>();
  });

  test("allows no local auth method (e.g. only a social method)", () => {
    expectTypeOf<typeof google>().toExtend<WaspSpec.AuthMethods>();
    expectTypeOf<typeof slack>().toExtend<WaspSpec.AuthMethods>();
    // eslint-disable-next-line @typescript-eslint/no-empty-object-type
    expectTypeOf<{}>().toExtend<WaspSpec.AuthMethods>();
  });

  test("allows a social method together with one local method", () => {
    expectTypeOf<
      typeof google & typeof usernameAndPassword
    >().toExtend<WaspSpec.AuthMethods>();
    expectTypeOf<
      typeof google & typeof email
    >().toExtend<WaspSpec.AuthMethods>();
  });

  test("forbids usernameAndPassword and email at the same time", () => {
    expectTypeOf<
      typeof usernameAndPassword & typeof email
    >().not.toExtend<WaspSpec.AuthMethods>();
  });

  test("forbids usernameAndPassword and email even alongside a social method", () => {
    expectTypeOf<
      typeof google & typeof usernameAndPassword & typeof email
    >().not.toExtend<WaspSpec.AuthMethods>();
  });
});

describe("Auth provider union", () => {
  const waspProvider = waspAuth({ methods: { usernameAndPassword: {} } });

  test("accepts a waspAuth() provider", () => {
    expectTypeOf<typeof waspProvider>().toExtend<WaspSpec.AuthProviderConfig>();
  });

  test("forbids waspAuth with no enabled methods", () => {
    expectTypeOf<{ methods: {} }>().not.toExtend<
      Parameters<typeof waspAuth>[0]
    >();
  });

  // The three tests below pin excess-property checking on literals: writing a
  // wasp-auth-only field directly on `auth` must be flagged at the site where
  // users actually write it (an object literal), which `@ts-expect-error`
  // asserts. Plain assignability cannot catch extra properties.
  test("forbids the old flat shape: methods on auth itself", () => {
    const _invalid: WaspSpec.Auth = {
      userEntity: "User",
      onAuthFailedRedirectTo: "/login",
      providers: [waspProvider],
      // @ts-expect-error -- methods only exists inside waspAuth()
      methods: { usernameAndPassword: {} },
    };
  });

  test("forbids wasp auth hooks on auth itself", () => {
    const _invalid: WaspSpec.Auth = {
      userEntity: "User",
      onAuthFailedRedirectTo: "/login",
      providers: [waspProvider],
      // @ts-expect-error -- hooks only exist inside waspAuth()
      onBeforeSignup: () => undefined,
    };
  });

  test("forbids onAuthSucceededRedirectTo on auth itself", () => {
    const _invalid: WaspSpec.Auth = {
      userEntity: "User",
      onAuthFailedRedirectTo: "/login",
      providers: [waspProvider],
      // @ts-expect-error -- onAuthSucceededRedirectTo only exists inside waspAuth()
      onAuthSucceededRedirectTo: "/",
    };
  });

  test("forbids a hand-written provider object where the union is expected", () => {
    expectTypeOf<{
      kind: "wasp";
      config: { methods: { usernameAndPassword: {} } };
    }>().not.toExtend<WaspSpec.AuthProviderConfig>();
  });

  test("forbids a manifest without the authenticity marker", () => {
    expectTypeOf<{
      kind: "external";
      contractVersion: 1;
      id: "external:clerk";
      server: { package: "@wasp.sh/auth-clerk/server" };
      capabilities: string[];
      env: { server: []; client: [] };
    }>().not.toExtend<WaspSpec.AuthProviderConfig>();
  });
});
