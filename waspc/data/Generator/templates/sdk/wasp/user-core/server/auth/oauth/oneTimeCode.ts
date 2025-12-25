import { createJWT, validateJWT, TimeSpan } from '../../../auth/jwt.js'

export const tokenStore = createTokenStore();

function createTokenStore() {
  const usedTokens = new Map<string, number>();

  const validFor = new TimeSpan(1, 'm') // 1 minute
  const cleanupAfter = 1000 * 60 * 60; // 1 hour

  function createToken(userId: string): Promise<string> {
    return createJWT(
      {
        id: userId,
      },
      {
        expiresIn: validFor,
      }
    );
  }

  function verifyToken(token: string): Promise<{ id: string }> {
    return validateJWT(token);
  }

  function isUsed(token: string): boolean {
    return usedTokens.has(token);
  }

  function markUsed(token: string): void {
    usedTokens.set(token, Date.now());
    cleanUp();
  }

  function cleanUp(): void {
    const now = Date.now();
    for (const [token, timestamp] of usedTokens.entries()) {
      if (now - timestamp > cleanupAfter) {
        usedTokens.delete(token);
      }
    }
  }

  return {
    createToken,
    verifyToken,
    isUsed,
    markUsed,
  };
}
