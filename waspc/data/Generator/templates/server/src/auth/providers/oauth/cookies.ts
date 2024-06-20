import {
  Request as ExpressRequest,
  Response as ExpressResponse,
} from "express";
import { parseCookies } from "oslo/cookie";

import type { ProviderConfig } from "wasp/auth/providers/types";

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
    // TODO: use server config to determine if secure
    secure: process.env.NODE_ENV === "production",
    path: "/",
    maxAge: 60 * 60 * 1000, // 1 hour
  });
}

export function getOAuthCookieValue(
  provider: ProviderConfig,
  req: ExpressRequest,
  fieldName: OAuthStateFieldName,
): string {
  const cookieName = `${provider.id}_${fieldName}`;
  const cookies = parseCookies(req.headers.cookie ?? "");
  return cookies.get(cookieName);
}
