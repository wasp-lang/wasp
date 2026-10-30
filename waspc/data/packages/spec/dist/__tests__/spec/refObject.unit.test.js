import { describe, expect, test } from "vitest";
import { mapRefObject } from "../../src/spec/refObject.js";
import { WaspSpecUserError } from "../../src/spec/waspSpecUserError.js";
import * as Fixtures from "./testFixtures.js";
describe("mapRefObject", () => {
    test("should map minimal named import correctly", () => {
        testMapRefObject(Fixtures.getRefObject("minimal", "named"));
    });
    test("should map full named import correctly", () => {
        testMapRefObject(Fixtures.getRefObject("full", "named"));
    });
    test("should map minimal default import correctly", () => {
        testMapRefObject(Fixtures.getRefObject("minimal", "default"));
    });
    test("should map full default import correctly", () => {
        testMapRefObject(Fixtures.getRefObject("full", "default"));
    });
    test.each([
        { refObject: () => null },
        { refObject: { parse: () => ({}) } },
        { refObject: { from: "./src/external" } },
    ])("returns an error for invalid runtime values", ({ refObject }) => {
        expect(() => mapRefObjectForProject(refObject)).toThrow(WaspSpecUserError);
        expect(() => mapRefObjectForProject(refObject)).toThrow("Got an import in the Wasp file that we couldn't process");
    });
    function testMapRefObject(refObject) {
        const result = mapRefObjectForProject(refObject);
        if ("import" in refObject) {
            expect(result).toStrictEqual({
                kind: "named",
                name: refObject.import,
                path: "@src/external",
                alias: refObject.alias,
            });
        }
        else {
            expect(result).toStrictEqual({
                kind: "default",
                name: refObject.importDefault,
                path: "@src/external",
            });
        }
    }
    function mapRefObjectForProject(refObject) {
        return mapRefObject(refObject, {
            projectRootDir: Fixtures.MOCK_PROJECT_DIR,
        });
    }
});
