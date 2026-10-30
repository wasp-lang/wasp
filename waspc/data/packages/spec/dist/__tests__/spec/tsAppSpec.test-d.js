// We are only interested in testing the types, so we don't actually need to use
// the variables we define here:
/* eslint-disable @typescript-eslint/no-unused-vars */
import { describe, expectTypeOf, test } from "vitest";
describe("Deployment", () => {
    test("allows an omitted mode or split mode", () => {
        // eslint-disable-next-line @typescript-eslint/no-empty-object-type -- An empty deployment object.
        expectTypeOf().toExtend();
        expectTypeOf().toExtend();
    });
});
describe("AuthMethods", () => {
    const usernameAndPassword = {
        usernameAndPassword: {},
    };
    const email = {
        email: {
            fromField: { email: "noreply@example.com" },
            emailVerification: { clientRoute: "/verify" },
            passwordReset: { clientRoute: "/reset" },
        },
    };
    const google = {
        google: {},
    };
    const slack = {
        slack: {},
    };
    test("allows only usernameAndPassword", () => {
        expectTypeOf().toExtend();
    });
    test("allows only email", () => {
        expectTypeOf().toExtend();
    });
    test("allows no local auth method (e.g. only a social method)", () => {
        expectTypeOf().toExtend();
        expectTypeOf().toExtend();
        // eslint-disable-next-line @typescript-eslint/no-empty-object-type
        expectTypeOf().toExtend();
    });
    test("allows a social method together with one local method", () => {
        expectTypeOf().toExtend();
        expectTypeOf().toExtend();
    });
    test("forbids usernameAndPassword and email at the same time", () => {
        expectTypeOf().not.toExtend();
    });
    test("forbids usernameAndPassword and email even alongside a social method", () => {
        expectTypeOf().not.toExtend();
    });
});
