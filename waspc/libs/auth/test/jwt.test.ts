import { describe, expect, it } from "vitest";
import { createCreateJWT, createValidateJWT, TimeSpan } from "../src/jwt";

describe("jwt helpers", () => {
  const secret = new TextEncoder().encode("my-secret-key");
  describe("createCreateJWT", () => {
    const createJWT = createCreateJWT(secret, "HS256");
    it("should return a function", async () => {
      expect(createJWT).toBeDefined();
      expect(typeof createJWT).toBe("function");
    });
    it("should create a JWT", async () => {
      const token = await createJWT(
        { sub: "test" },
        { expiresIn: new TimeSpan(2, "d") },
      );
      expect(token).toBeDefined();
      expect(typeof token).toBe("string");
      // JWT has 3 parts
      expect(token.split(".").length).toBe(3);
    });
  });

  describe("createValidateJWT", () => {
    const validateJWT = createValidateJWT(secret, "HS256");
    it("should return a function", async () => {
      expect(validateJWT).toBeDefined();
      expect(typeof validateJWT).toBe("function");
    });
    it("should validate a JWT", async () => {
      const jwt = await createCreateJWT(secret, "HS256")(
        { sub: "test" },
        { expiresIn: new TimeSpan(2, "d") },
      );
      const payload = await validateJWT<{ sub: string }>(jwt);
      expect(payload).toBeDefined();
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
