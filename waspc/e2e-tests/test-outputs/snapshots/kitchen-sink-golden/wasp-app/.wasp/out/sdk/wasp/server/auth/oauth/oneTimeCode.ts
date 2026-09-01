import { createJWT, validateJWT, TimeSpan } from '../jwt.js'

export const tokenStore = createTokenStore();

function createTokenStore() {
  const usedTokens = new Map<string, number>();

  const validFor = new TimeSpan(1, 'm') // 1 minute
  const cleanupAfter = 1000 * 60 * 60; // 1 hour

  // The code names the SUBJECT (namespace + provider user id), not an auth
  // id: redeeming it goes through the same `wasp-sessions` facet an adapter
  // package uses, and that facet is subject-addressed by design.
  function createToken(subject: { namespace: string; subjectId: string }): Promise<string> {
    return createJWT(
      subject,
      {
        expiresIn: validFor,
      }
    );
  }

  function verifyToken(token: string): Promise<{ namespace: string; subjectId: string }> {
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
