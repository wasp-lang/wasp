import {
  Response as ExpressResponse,
  Request as ExpressRequest,
} from 'express';
import { generateCodeVerifier, generateState } from 'arctic';

import type { ProviderConfig } from 'wasp/auth/providers/types';

import { setOAuthCookieValue, getOAuthCookieValue } from './cookies.js';

export type RequiredStateType = 'state';

export type OptionalStateType = 'codeVerifier';

export function generateAndStoreOAuthState<OST extends OptionalStateType>(
  optionalStateTypes: OST[],
  provider: ProviderConfig,
  res: ExpressResponse
): { [name in OST]: string } & { [name in RequiredStateType]: string } {
  const result = {} as {
    [name in OptionalStateType | RequiredStateType]: string;
  };

  const state = generateState();
  setOAuthCookieValue(provider, res, 'state', state);
  result.state = state;

  if (optionalStateTypes.includes('codeVerifier' as ST)) {
    const codeVerifier = generateCodeVerifier();
    setOAuthCookieValue(provider, res, 'codeVerifier', codeVerifier);
    result.codeVerifier = codeVerifier;
  }

  return result;
}

export function validateAndGetOAuthState<OST extends OptionalStateType>(
  optionalStateTypes: OST[],
  provider: ProviderConfig,
  req: ExpressRequest
): { [name in OST]: string } & { [name in RequiredStateType]: string } & {
  code: string;
} {
  const result = {} as {
    [name in OptionalStateType]: string;
  } & {
    [name in RequiredStateType]: string;
  } & {
    code: string;
  };

  const state = req.query.state;
  const storedState = getOAuthCookieValue(provider, req, 'state');
  if (!state || !storedState || storedState !== state) {
    throw new Error('Invalid state');
  }
  result.state = storedState;

  if (optionalStateTypes.includes('codeVerifier' as OST)) {
    const storedCodeVerifier = getOAuthCookieValue(
      provider,
      req,
      'codeVerifier'
    );
    if (!storedCodeVerifier) {
      throw new Error('Invalid code verifier');
    }
    result.codeVerifier = storedCodeVerifier;
  }

  const code = req.query.code;
  if (typeof code !== 'string') {
    throw new Error('Invalid code');
  }
  result.code = code;

  return result;
}
