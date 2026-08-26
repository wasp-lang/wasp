import { createJWT, TimeSpan, validateJWT } from "../jwt.js";

export const sessionHandoffCodes = createSessionHandoffCodes();

function createSessionHandoffCodes() {
  const redeemedCodes = new Map<string, number>();

  const validFor = new TimeSpan(1, "m"); // 1 minute
  const cleanupAfter = 1000 * 60 * 60; // 1 hour

  function issue(authId: string): Promise<string> {
    return createJWT(
      {
        id: authId,
      },
      {
        expiresIn: validFor,
      },
    );
  }

  async function redeem(code: string): Promise<string | null> {
    const { id: authId } = await validateJWT(code);
    if (redeemedCodes.has(code)) {
      return null;
    }
    redeemedCodes.set(code, Date.now());
    cleanUp();
    return authId;
  }

  function cleanUp(): void {
    const now = Date.now();
    for (const [code, timestamp] of redeemedCodes.entries()) {
      if (now - timestamp > cleanupAfter) {
        redeemedCodes.delete(code);
      }
    }
  }

  return {
    issue,
    redeem,
  };
}
