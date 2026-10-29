import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { createJWTHelpers, TimeSpan } from "../src/node/jwt";

const secret = new TextEncoder().encode("my-secret-key");
const { createJWT, validateJWT } = createJWTHelpers(secret, "HS256");
const defaultOptions = {
  expiresIn: new TimeSpan(1, "h"),
};

describe("jwt helpers", () => {
  beforeEach(() => {
    // Tell vitest we use mocked time.
    vi.useFakeTimers();
  });

  afterEach(() => {
    // Restoring date after each test run.
    vi.useRealTimers();
  });

  const exampleJwtSub = "test";
  const exampleCreationTime = new Date("2025-01-01T00:00:00Z");
  const validJWTForExampleSubAndCreationTime =
    "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJ0ZXN0IiwiZXhwIjoxNzM1NjkzMjAwfQ.K6XyTELjkuNoiJFIhdsIB58IWA8BiyIJ7M2SIGUTHSg";

  describe("createJWT", () => {
    it("should create a JWT", async () => {
      vi.setSystemTime(exampleCreationTime);
      const token = await createJWT({ sub: exampleJwtSub }, defaultOptions);

      expect(token).toEqual(validJWTForExampleSubAndCreationTime);
    });
  });

  describe("validateJWT", () => {
    it("should validate a JWT", async () => {
      const jwt = await createJWT({ sub: exampleJwtSub }, defaultOptions);

      const payload = await validateJWT<{ sub: string }>(jwt);

      expect(payload.sub).toBe(exampleJwtSub);
    });
    it("should throw an error for an expired JWT", async () => {
      vi.setSystemTime(exampleCreationTime);
      const jwt = await createJWT({ sub: exampleJwtSub }, defaultOptions);

      const twoHoursLater = new Date(exampleCreationTime);
      twoHoursLater.setHours(twoHoursLater.getHours() + 2);
      vi.setSystemTime(twoHoursLater);

      await expect(validateJWT<{ sub: string }>(jwt)).rejects.toThrow(
        /Expired JWT/,
      );
    });
    testTamperingWithPartOfJWTToken("header", 0, /Unexpected/);
    testTamperingWithPartOfJWTToken("payload", 1, /Unexpected/);
    testTamperingWithPartOfJWTToken("signature", 2, /Invalid signature/);
  });
});

function testTamperingWithPartOfJWTToken(
  partName: string,
  partIndex: number,
  expectedError: RegExp,
) {
  it(`should throw an error when ${partName} is invalid`, async () => {
    const jwt = await createJWT({ sub: "some-value" }, defaultOptions);
    const tamperedJWT = modifyPartOfJWTToken(jwt, partIndex, "abc");

    await expect(validateJWT<{ sub: string }>(tamperedJWT)).rejects.toThrow(
      expectedError,
    );
  });
}

function modifyPartOfJWTToken(
  jwt: string,
  partIndex: number,
  newValue: string,
): string {
  const parts = jwt.split(".");
  parts[partIndex] = newValue;
  return parts.join(".");
}
