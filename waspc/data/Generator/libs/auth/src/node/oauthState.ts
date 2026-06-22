import { parseCookies } from "oslo/cookie";
import {
  generateCodeVerifier as generateOAuthCodeVerifier,
  generateState as generateOAuthState,
} from "oslo/oauth2";

export type OAuthStateFor<OT extends OAuthType> = OAuthStateForOAuthType[OT];

export type OAuthStateWithCodeFor<OT extends OAuthType> = OAuthStateFor<OT> &
  OAuthCode;

export type OAuthType = keyof OAuthStateForOAuthType;

export type OAuthStateFieldName = keyof OAuthState | keyof OAuthStateWithPKCE;

type OAuthStateForOAuthType = {
  OAuth2: OAuthState;
  OAuth2WithPKCE: OAuthStateWithPKCE;
};

type OAuthState = {
  state: string;
};

type OAuthStateWithPKCE = {
  state: string;
  codeVerifier: string;
};

type OAuthCode = {
  code: string;
};

type OAuthStateStore = {
  set(fieldName: OAuthStateFieldName, value: string): void;
  get(fieldName: OAuthStateFieldName): string | undefined;
};

export type OAuthCookieOptions = {
  httpOnly: true;
  secure: boolean;
  path: "/";
  maxAge: number;
};

export function generateAndStoreOAuthState<OT extends OAuthType>({
  oAuthType,
  store,
}: {
  oAuthType: OT;
  store: OAuthStateStore;
}): OAuthStateFor<OT> {
  const state = (
    oAuthType === "OAuth2WithPKCE"
      ? { ...generateState(), ...generateCodeVerifier() }
      : generateState()
  ) as OAuthStateFor<OT>;

  storeOAuthState(store, state);

  return state;
}

export function validateAndGetOAuthState<OT extends OAuthType>({
  oAuthType,
  code,
  state,
  store,
}: {
  oAuthType: OT;
  code: unknown;
  state: unknown;
  store: OAuthStateStore;
}): OAuthStateWithCodeFor<OT> {
  const oAuthState = (
    oAuthType === "OAuth2WithPKCE"
      ? { ...getCode(code), ...getState(state), ...getCodeVerifier(store) }
      : { ...getCode(code), ...getState(state) }
  ) as OAuthStateWithCodeFor<OT>;

  validateOAuthState(store, oAuthState);

  return oAuthState;
}

export function getOAuthCookieName(
  providerId: string,
  fieldName: OAuthStateFieldName,
): string {
  return `${providerId}_${fieldName}`;
}

export function getOAuthCookieOptions({
  isDevelopment,
}: {
  isDevelopment: boolean;
}): OAuthCookieOptions {
  return {
    httpOnly: true,
    secure: !isDevelopment,
    path: "/",
    maxAge: 60 * 60 * 1000,
  };
}

export function getOAuthCookieValueFromHeader(
  providerId: string,
  fieldName: OAuthStateFieldName,
  cookieHeader: string,
): string {
  const cookieName = getOAuthCookieName(providerId, fieldName);
  const cookies = parseCookies(cookieHeader);
  return cookies.get(cookieName) as string;
}

function storeOAuthState(
  store: OAuthStateStore,
  state: OAuthStateFor<OAuthType>,
): void {
  let key: keyof typeof state;
  for (key in state) {
    store.set(key, state[key]);
  }
}

function validateOAuthState(
  store: OAuthStateStore,
  state: OAuthStateWithCodeFor<OAuthType>,
): void {
  if (typeof state.code !== "string") {
    throw new Error("Invalid code");
  }

  const storedState = store.get("state");
  if (!state.state || !storedState || storedState !== state.state) {
    throw new Error("Invalid state");
  }

  if (isOAuthStateWithPKCE(state) && !state.codeVerifier) {
    throw new Error("Missing code verifier");
  }
}

function generateState(): { state: string } {
  return { state: generateOAuthState() };
}

function generateCodeVerifier(): { codeVerifier: string } {
  return { codeVerifier: generateOAuthCodeVerifier() };
}

function getCode(code: unknown): { code: string } {
  return { code: code as string };
}

function getState(state: unknown): { state: string } {
  return { state: state as string };
}

function getCodeVerifier(store: OAuthStateStore): { codeVerifier: string } {
  return { codeVerifier: store.get("codeVerifier") ?? "" };
}

function isOAuthStateWithPKCE(
  state: OAuthState | OAuthStateWithPKCE,
): state is OAuthStateWithPKCE {
  return "codeVerifier" in state;
}
