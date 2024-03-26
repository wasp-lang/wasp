import {
  Response as ExpressResponse,
  Request as ExpressRequest,
} from "express";
import { generateCodeVerifier, generateState } from "arctic";

import type { ProviderConfig } from "wasp/auth/providers/types";

import { setOAuthCookieValue, getOAuthCookieValue } from "./cookies.js";

export type StateType = 'state' | 'codeVerifier';

export function generateAndStoreOAuthState<ST extends StateType>(
  stateTypes: ST[],
  provider: ProviderConfig,
  res: ExpressResponse,
): { [name in ST]: string } {
  const result = {} as { [name in StateType]: string }

  if (stateTypes.includes('state' as ST)) {
    const state = generateState();
    setOAuthCookieValue(provider, res, 'state', state);
    result.state = state;
  }

  if (stateTypes.includes('codeVerifier' as ST)) {
    const codeVerifier = generateCodeVerifier();
    setOAuthCookieValue(provider, res, 'codeVerifier', codeVerifier);
    result.codeVerifier = codeVerifier;
  }

  return result;
}

export function validateAndGetOAuthState<ST extends StateType>(
  stateTypes: ST[],
  provider: ProviderConfig,
  req: ExpressRequest,
): { [name in ST]: string } & { code: string } {
  const result = {} as { [name in StateType]: string } & { code: string };

  if (stateTypes.includes('state' as ST)) {
    const state = req.query.state;
    const storedState = getOAuthCookieValue(provider, req, 'state');
    if (
      !state ||
      !storedState ||
      storedState !== state
    ) {
      throw new Error("Invalid state");
    }
    result.state = storedState;
  }

  if (stateTypes.includes('codeVerifier' as ST)) {
    const storedCodeVerifier = getOAuthCookieValue(provider, req, 'codeVerifier');
    if (!storedCodeVerifier) {
      throw new Error("Invalid code verifier");
    }
    result.codeVerifier = storedCodeVerifier;
  }

  const code = req.query.code;
  if (typeof code !== "string") {
    throw new Error("Invalid code");
  }
  result.code = code;

  return result;
}
