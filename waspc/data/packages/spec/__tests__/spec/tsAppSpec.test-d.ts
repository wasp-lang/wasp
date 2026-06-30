// We are only interested in testing the types, so we don't actually need to use
// the variables we define here:
/* eslint-disable @typescript-eslint/no-unused-vars */

import { describe, expectTypeOf, test } from "vitest";
import type * as WaspSpec from "../../src/spec/publicApi/waspSpec.js";

describe("AuthMethods", () => {
  const usernameAndPassword: Required<
    Pick<WaspSpec.AuthMethods, "usernameAndPassword">
  > = {
    usernameAndPassword: {},
  };

  const someRoute: WaspSpec.Route = {
    kind: "route",
    name: "SomeRoute",
    path: "/some",
    page: { kind: "page", component: () => null },
  };

  const email: Required<Pick<WaspSpec.AuthMethods, "email">> = {
    email: {
      fromField: { email: "noreply@example.com" },
      emailVerification: { clientRoute: someRoute },
      passwordReset: { clientRoute: someRoute },
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
