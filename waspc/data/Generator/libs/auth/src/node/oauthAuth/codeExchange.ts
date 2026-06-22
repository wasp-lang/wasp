import {
  throwInvalidOAuthCode,
  throwMissingOAuthCode,
  throwUsedOAuthCode,
} from "./errors";
import type { OAuthCodeExchangeArgs, OAuthCodeExchangeResult } from "./types";

export async function exchangeOAuthCodeForSession<User>({
  fields,
  adapters,
}: OAuthCodeExchangeArgs<User>): Promise<OAuthCodeExchangeResult> {
  const { code } = fields as { code?: string };

  if (code === undefined) {
    throwMissingOAuthCode();
  }

  if (adapters.oneTimeCodeStore.isUsed(code)) {
    throwUsedOAuthCode();
  }

  const { authId } = await adapters.oneTimeCodeStore
    .verifyToken(code)
    .catch(() => {
      throwInvalidOAuthCode();
    });
  const auth = await adapters.authRepository.findAuthWithUserByAuthId(authId);

  if (auth === null) {
    throwInvalidOAuthCode();
  }

  const session = await adapters.sessionService.createSession(auth.authId);
  adapters.oneTimeCodeStore.markUsed(code);

  return { sessionId: session.id };
}
