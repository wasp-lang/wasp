import {
  Request as ExpressRequest,
  Response as ExpressResponse,
} from "express";
import { parseCookies } from "oslo/cookie";

import { type ProviderConfig } from "wasp/auth/providers/types";

import { type StateType } from './state';

export function setOAuthCookieValue(
  provider: ProviderConfig,
  res: ExpressResponse,
  stateType: StateType,
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
  stateType: StateType,
) {
  const cookieName = `${provider.id}_${stateType}`;
  const cookies = parseCookies(req.headers.cookie ?? "");
  return cookies.get(cookieName);
}
