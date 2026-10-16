import {
  Request as ExpressRequest,
  Response as ExpressResponse,
} from 'express';
import { parseCookies } from '@wasp.sh/lib-auth/node';

import type { ProviderConfig } from 'wasp/auth/providers/types';
import { config } from 'wasp/server';

import type { OAuthStateFieldName } from './state';

export function setOAuthCookieValue(
  provider: ProviderConfig,
  res: ExpressResponse,
  fieldName: OAuthStateFieldName,
  value: string,
): void {
  const cookieName = `${provider.id}_${fieldName}`;
  res.cookie(cookieName, value, {
    httpOnly: true,
    secure: !config.isDevelopment,
    // The OAuth callback is a top-level GET navigation, which `lax` permits.
    // We set it explicitly because an omitted `sameSite` is not `lax` in every
    // browser.
    sameSite: "lax",
    path: "/",
    maxAge: 60 * 60 * 1000, // 1 hour
  });
}

export function getOAuthCookieValue(
  provider: ProviderConfig,
  req: ExpressRequest,
  fieldName: OAuthStateFieldName,
): string | undefined {
  const cookieName = `${provider.id}_${fieldName}`;
  const cookies = parseCookies(req.headers.cookie ?? "");
  return cookies.get(cookieName);
}
