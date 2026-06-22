import {
  Response as ExpressResponse,
  Request as ExpressRequest,
} from 'express';
import {
  generateAndStoreOAuthState as generateAndStoreLibOAuthState,
  validateAndGetOAuthState as validateAndGetLibOAuthState,
  type OAuthStateFieldName,
  type OAuthStateFor,
  type OAuthStateWithCodeFor,
  type OAuthType,
} from '@wasp.sh/lib-auth/node';

import type { ProviderConfig } from 'wasp/auth/providers/types';

import { setOAuthCookieValue, getOAuthCookieValue } from './cookies.js';

export type {
  OAuthStateFieldName,
  OAuthStateFor,
  OAuthStateWithCodeFor,
  OAuthType,
};

export function generateAndStoreOAuthState<OT extends OAuthType>({
  oAuthType,
  provider,
  res,
}: {
  oAuthType: OT,
  provider: ProviderConfig,
  res: ExpressResponse
}): OAuthStateFor<OT> {
  return generateAndStoreLibOAuthState({
    oAuthType,
    store: {
      set: (fieldName, value) => setOAuthCookieValue(provider, res, fieldName, value),
      get: () => undefined,
    },
  });
}

export function validateAndGetOAuthState<OT extends OAuthType>({
  oAuthType,
  provider,
  req,
}: {
  oAuthType: OT,
  provider: ProviderConfig,
  req: ExpressRequest
}): OAuthStateWithCodeFor<OT> {
  return validateAndGetLibOAuthState({
    oAuthType,
    code: `${req.query.code}`,
    state: `${req.query.state}`,
    store: {
      set: () => {},
      get: (fieldName) => getOAuthCookieValue(provider, req, fieldName),
    },
  });
}
