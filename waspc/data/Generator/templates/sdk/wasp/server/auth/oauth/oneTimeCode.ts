import { createJWT, validateJWT, TimeSpan } from '../jwt.js'
import { prisma } from '../../index.js'

export const tokenStore = createTokenStore();

/**
 * One-time login codes for the OAuth handback: the callback mints a
 * short-lived JWT naming the subject, the client redeems it once for a
 * session.
 *
 * Replay protection is DB-backed: a code is spent by inserting its row, so
 * two concurrent redemptions are settled by the primary key -- whichever
 * server instance they hit. Spent rows outlive the JWT's validity and are
 * deleted lazily.
 */
function createTokenStore() {
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

  /**
   * Spends the code: true exactly once per code, false on every replay.
   * Insert-then-catch rather than check-then-insert, so the uniqueness
   * constraint settles concurrent redemptions.
   */
  async function tryMarkUsed(token: string): Promise<boolean> {
    await cleanUp();
    try {
      // The model name is fixed by the generator's schema injection
      // (Wasp.Generator.DbGenerator.Auth.usedOneTimeCodeEntityName).
      await prisma.usedOneTimeCode.create({ data: { code: token } });
      return true;
    } catch (e: unknown) {
      if (
        typeof e === 'object' && e !== null && 'code' in e && (e as { code: unknown }).code === 'P2002'
      ) {
        return false;
      }
      throw e;
    }
  }

  async function cleanUp(): Promise<void> {
    await prisma.usedOneTimeCode.deleteMany({
      where: { usedAt: { lt: new Date(Date.now() - cleanupAfter) } },
    });
  }

  return {
    createToken,
    verifyToken,
    tryMarkUsed,
  };
}
