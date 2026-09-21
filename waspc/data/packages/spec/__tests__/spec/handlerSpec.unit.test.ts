import { describe, expect, test } from "vitest";
import { splitHandlerSpec } from "../../src/spec/mapper/handlerSpec.js";
import * as Fixtures from "./testFixtures.js";

const ref = () => Fixtures.getRefObject("full", "named");

describe("splitHandlerSpec", () => {
  test("no spec means no data and no references", () => {
    expect(splitHandlerSpec("s", "server", undefined)).toStrictEqual({
      dataJson: undefined,
      references: {},
    });
  });

  test("lifts references out by path and leaves the data in place", () => {
    const configFn = ref();
    const getEmailContent = ref();
    const result = splitHandlerSpec("s", "server", {
      redirectTo: "/",
      methods: { google: { scopes: ["profile"], configFn } },
      getEmailContent,
    });
    expect(JSON.parse(result.dataJson!)).toStrictEqual({
      redirectTo: "/",
      methods: { google: { scopes: ["profile"] } },
    });
    expect(result.references).toStrictEqual({
      '["methods","google","configFn"]': configFn,
      '["getEmailContent"]': getEmailContent,
    });
  });

  test("a key containing a dot is one path segment, not nesting", () => {
    const fn = ref();
    const result = splitHandlerSpec("s", "server", { "a.b": { fn } });
    expect(Object.keys(result.references)).toStrictEqual(['["a.b","fn"]']);
  });

  test("rejects a reference inside an array", () => {
    expect(() =>
      splitHandlerSpec("s", "client", { components: [ref()] }),
    ).toThrow(/reference inside an array at 'components'/);
  });

  test("rejects data that does not survive JSON", () => {
    expect(() => splitHandlerSpec("s", "server", { when: new Date() })).toThrow(
      /does not survive JSON serialization/,
    );
    expect(() => splitHandlerSpec("s", "server", { fn: () => 1 })).toThrow(
      /does not survive JSON serialization/,
    );
  });

  test("rejects a spec that is not a plain object", () => {
    expect(() => splitHandlerSpec("s", "server", [1, 2])).toThrow(
      /not a plain object/,
    );
    expect(() => splitHandlerSpec("s", "server", ref())).toThrow(
      /not a plain object/,
    );
  });
});
