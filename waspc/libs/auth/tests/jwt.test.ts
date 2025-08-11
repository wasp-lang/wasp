import { describe, expect, it } from "vitest";
import { createJWTHelpers, TimeSpan } from "../src/jwt";

describe("jwt helpers", () => {
  const secret = new TextEncoder().encode("my-secret-key");
  const { createJWT, validateJWT } = createJWTHelpers(secret, "HS256");
  describe("createJWT", () => {
    it("should create a JWT", async () => {
      const token = await createJWT(
        { sub: "test" },
        { expiresIn: new TimeSpan(2, "d") },
      );
      // JWT has 3 parts
      expect(token.split(".").length).toBe(3);
    });
  });

  describe("validateJWT", () => {
    it("should validate a JWT", async () => {
      const jwt = await createJWT(
        { sub: "test" },
        { expiresIn: new TimeSpan(2, "d") },
      );
      const payload = await validateJWT<{ sub: string }>(jwt);
      expect(payload.sub).toBe("test");
    });
    it("should throw an error for an invalid JWT", async () => {
      const invalidJWT = "invalid.jwt.token";
      await expect(validateJWT<{ sub: string }>(invalidJWT)).rejects.toThrow(
        /Unexpected token/,
      );
    });
  });
});
