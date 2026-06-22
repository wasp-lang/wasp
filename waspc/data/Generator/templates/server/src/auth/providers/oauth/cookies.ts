import {
  Request as ExpressRequest,
  Response as ExpressResponse,
} from 'express';
import {
  getOAuthCookieName,
  getOAuthCookieOptions,
  getOAuthCookieValueFromHeader,
} from '@wasp.sh/lib-auth/node';

import type { ProviderConfig } from 'wasp/auth/providers/types';
import { config } from 'wasp/server';

import type { OAuthStateFieldName } from './state';

export function setOAuthCookieValue(
  provider: ProviderConfig,
  res: ExpressResponse,
  fieldName: OAuthStateFieldName,
  value: string,
): void {
  const cookieName = getOAuthCookieName(provider.id, fieldName);
  res.cookie(cookieName, value, {
    ...getOAuthCookieOptions({ isDevelopment: config.isDevelopment }),
  });
}

export function getOAuthCookieValue(
  provider: ProviderConfig,
  req: ExpressRequest,
  fieldName: OAuthStateFieldName,
): string {
  return getOAuthCookieValueFromHeader(
    provider.id,
    fieldName,
    req.headers.cookie ?? "",
  );
}
