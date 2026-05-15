// We are only interested in testing the types, so we don't actually need to use
// the variables we define here:
/* eslint-disable @typescript-eslint/no-unused-vars */

import { describe, expectTypeOf, test } from "vitest";
import type * as TsAppSpec from "../../src/spec/publicApi/tsAppSpec.js";

describe("AuthMethods", () => {
  const usernameAndPassword = {
    usernameAndPassword: {},
  } satisfies Pick<TsAppSpec.AuthMethods, "usernameAndPassword">;

  const email = {
    email: {
      fromField: { email: "noreply@example.com" },
      emailVerification: { clientRoute: "/verify" },
      passwordReset: { clientRoute: "/reset" },
    },
  } satisfies Pick<TsAppSpec.AuthMethods, "email">;

  const google = {
    google: {},
  } satisfies Pick<TsAppSpec.AuthMethods, "google">;

  test("allows only usernameAndPassword", () => {
    expectTypeOf<
      typeof usernameAndPassword
    >().toExtend<TsAppSpec.AuthMethods>();
  });

  test("allows only email", () => {
    expectTypeOf<typeof email>().toExtend<TsAppSpec.AuthMethods>();
  });

  test("allows no local auth method (e.g. only a social method)", () => {
    expectTypeOf<typeof google>().toExtend<TsAppSpec.AuthMethods>();
    // eslint-disable-next-line @typescript-eslint/no-empty-object-type
    expectTypeOf<{}>().toExtend<TsAppSpec.AuthMethods>();
  });

  test("allows a social method together with one local method", () => {
    expectTypeOf<
      typeof google & typeof usernameAndPassword
    >().toExtend<TsAppSpec.AuthMethods>();
    expectTypeOf<
      typeof google & typeof email
    >().toExtend<TsAppSpec.AuthMethods>();
  });

  test("forbids usernameAndPassword and email at the same time", () => {
    expectTypeOf<
      typeof usernameAndPassword & typeof email
    >().not.toExtend<TsAppSpec.AuthMethods>();
  });

  test("forbids usernameAndPassword and email even alongside a social method", () => {
    expectTypeOf<
      typeof google & typeof usernameAndPassword & typeof email
    >().not.toExtend<TsAppSpec.AuthMethods>();
  });
});
