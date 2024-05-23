import {
  Request as ExpressRequest,
  Response as ExpressResponse,
} from "express";
import { parseCookies } from "oslo/cookie";

import type { ProviderConfig } from "wasp/auth/providers/types";

import type { OptionalStateType, RequiredStateType } from './state';

export function setOAuthCookieValue(
  provider: ProviderConfig,
  res: ExpressResponse,
  stateType: RequiredStateType | OptionalStateType,
  value: string,
) {
  const cookieName = `${provider.id}_${stateType}`;
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
  stateType: RequiredStateType | OptionalStateType,
) {
  const cookieName = `${provider.id}_${stateType}`;
  const cookies = parseCookies(req.headers.cookie ?? "");
  return cookies.get(cookieName);
}
