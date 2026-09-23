/**
 * Type-level tests for the typed adapters. Checked by `npm test` (`tsc`);
 * every `@ts-expect-error` is an assertion that the line does not compile.
 */

import type { ClientAuthAdapterFor } from "../client.js";
import type {
  AuthHandler,
  IdentityStore,
  OAuthLoginData,
  ServerAuthAdapterFor,
  ServerCredentialAuthAdapter,
  SpecReference,
  WaspServerRuntime,
} from "../index.js";

declare const handler: AuthHandler;
type OAuthConfigFn = () => { scopes: string[] };
declare function waspAuthT(p: { email?: boolean }): {
  server: {
    spec: {
      google?: { scopes: string[]; configFn?: SpecReference<OAuthConfigFn> };
    };
    env: Array<{
      name: "GOOGLE_CLIENT_ID" | "GOOGLE_CLIENT_SECRET";
      optional?: boolean;
    }>;
    routes: {};
  };
  client: { spec: { methods: string[] }; env: [{ name: "REACT_APP_X" }] };
  uses: Array<"email-send">;
  providers: { email?: {}; google?: { kind: "oauth" } };
  credentials: { transport?: "bearer" | "cookie" };
};
export const wasp: ServerAuthAdapterFor<typeof waspAuthT> = (runtime, spec) => {
  runtime.credentialsIssuer.signIn({ providerUserId: "a" });
  const scopes: string[] =
    spec.google?.configFn?.().scopes ?? spec.google?.scopes ?? [];
  // @ts-expect-error the adapter gets the live function, not the reference
  spec.google?.configFn?.kind;
  // @ts-expect-error app decides
  runtime.email.send;
  if (runtime.canSendEmail) runtime.email.defaultFrom;
  const id: string | undefined = runtime.env.GOOGLE_CLIENT_ID;
  // @ts-expect-error array env is never a plain string
  const id2: string = runtime.env.GOOGLE_CLIENT_ID;
  // @ts-expect-error undeclared
  runtime.env.DATABASE_URL;
  runtime.identities.email;
  runtime.mountPath;
  // @ts-expect-error undeclared provider name
  runtime.identities.github;
  return { handler, routeHandler: () => new Response() };
};
// @ts-expect-error routes declared
export const waspNoRoutes: ServerAuthAdapterFor<typeof waspAuthT> = () => ({
  handler,
});
export const waspClient: ClientAuthAdapterFor<typeof waspAuthT> = (
  runtime,
  spec,
) => {
  const x: string = runtime.env.REACT_APP_X;
  spec.methods;
  runtime.mountUrl;
  // @ts-expect-error undeclared
  runtime.env.OTHER;
  return {};
};
declare function clerkT(): {
  server: {
    spec: {};
    env: [{ name: "CLERK_SECRET_KEY" }, { name: "OPT"; optional: true }];
  };
};
export const clerkA: ServerAuthAdapterFor<typeof clerkT> = (runtime) => {
  const key: string = runtime.env.CLERK_SECRET_KEY;
  // @ts-expect-error optional
  const opt: string = runtime.env.OPT;
  // @ts-expect-error never
  runtime.credentialsIssuer;
  // @ts-expect-error never
  runtime.email;
  const no: false = runtime.hasCredentialsIssuer;
  runtime.identities.default;
  return { handler };
};
export const clerkR: ServerAuthAdapterFor<typeof clerkT> = () => ({
  handler,
  // @ts-expect-error no routes declared, so Wasp would never mount it
  routeHandler: () => new Response(),
});
declare function baT(): {
  server: { routes?: { rawBody: true } };
  credentials?: {};
  uses: ["email-send"];
};
export const ba: ServerAuthAdapterFor<typeof baT> = (runtime, spec) => {
  const s: unknown = spec;
  runtime.email.send;
  // @ts-expect-error app decides
  runtime.credentialsIssuer;
  if (runtime.hasCredentialsIssuer) runtime.credentialsIssuer.signOut;
  return { handler };
};
// a fully typed runtime can be handed to code written against the loose one
export const loose: ServerAuthAdapterFor<typeof waspAuthT> = (runtime) => {
  if (runtime.canSendEmail) {
    const r: WaspServerRuntime<"email" | "google"> = runtime;
  }
  return { handler, routeHandler: () => new Response() };
};

// the app side: a ref object fits a reference field, whatever copy of the spec package branded it
declare const appRef: {
  import: string;
  from: string;
  kind: "refObject";
  readonly __brand: "RefObject";
};
const referenceField: SpecReference<OAuthConfigFn> = appRef;
// @ts-expect-error a plain function is not a reference
const notAReference: SpecReference<OAuthConfigFn> = () => ({ scopes: [] });

// ---- provider kinds: what each store call and sign-in must carry ----
declare const oauth: OAuthLoginData;
declare function kindsAuth(): {
  server: { routes: {} };
  providers: {
    email?: {};
    username?: {};
    google?: { kind: "oauth" };
    flexible: { kind?: "oauth" };
  };
  credentials: {};
};
export const kindsTyped: ServerAuthAdapterFor<typeof kindsAuth> = async (
  runtime,
) => {
  const { identities, credentialsIssuer } = runtime;
  // oauth provider: data required
  await identities.google.create("id", { oauth });
  // @ts-expect-error missing oauth data
  await identities.google.create("id", {});
  // @ts-expect-error missing opts altogether
  await identities.google.create("id");
  await identities.google.link("id", { authId: "a", oauth });
  // @ts-expect-error link without oauth data
  await identities.google.link("id", { authId: "a" });
  await identities.google.provision("id", { oauth });
  // plain provider: data not accepted
  await identities.email.create("a@b.c");
  await identities.email.create("a@b.c", { getUserFields: () => ({}) });
  // @ts-expect-error a plain provider carries no oauth data
  await identities.email.create("a@b.c", { oauth });
  await identities.email.link("a@b.c", { authId: "a" });
  // app decides: optional
  await identities.flexible.create("x");
  await identities.flexible.create("x", { oauth });
  // @ts-expect-error undeclared provider
  identities.github;
  // issuer: kind looked up from the ref's provider name
  await credentialsIssuer.signIn(
    { providerName: "google", providerUserId: "id" },
    { oauth },
  );
  // @ts-expect-error oauth sign-in without data
  await credentialsIssuer.signIn({
    providerName: "google",
    providerUserId: "id",
  });
  await credentialsIssuer.signIn({
    providerName: "email",
    providerUserId: "a@b.c",
  });
  await credentialsIssuer.signIn(
    { providerName: "email", providerUserId: "a@b.c" },
    // @ts-expect-error plain sign-in carries no oauth data
    { oauth },
  );
  await credentialsIssuer.signIn({
    // @ts-expect-error undeclared provider name
    providerName: "github",
    providerUserId: "x",
  });
  await credentialsIssuer.signOutEverywhere({
    providerName: "email",
    providerUserId: "a@b.c",
  });
  return { handler, routeHandler: () => new Response() };
};
// no providers declared: just `default`, plain
declare function plainAuth(): { server: {} };
export const kindsPlain: ServerAuthAdapterFor<typeof plainAuth> = async (
  runtime,
) => {
  await runtime.identities.default.provision("user_1", {
    identity: { claims: {} },
  });
  return { handler };
};
// loose: everything optional, shape still checked
export const kindsLoose: ServerCredentialAuthAdapter = async (runtime) => {
  await runtime.identities.default.create("x");
  await runtime.identities.default.create("x", { oauth });
  await runtime.identities.default.create("x", {
    // @ts-expect-error wrong shape
    oauth: { tokens: 1 },
  });
  await runtime.credentialsIssuer.signIn({ providerUserId: "x" });
  await runtime.credentialsIssuer.signIn(
    { providerName: "anything", providerUserId: "x" },
    { oauth },
  );
  return { handler };
};
// a typed runtime can be handed to code written against the loose store
export const kindsMix: ServerAuthAdapterFor<typeof kindsAuth> = (runtime) => {
  const anyStore: IdentityStore = runtime.identities.google;
  void anyStore;
  return { handler, routeHandler: () => new Response() };
};

// ---- the two kinds: a login handler returns routes only, a credential handler its AuthHandler ----
declare function loginAuth(): {
  kind: "login";
  server: { routes: {} };
  credentials: {};
};
export const loginTyped: ServerAuthAdapterFor<typeof loginAuth> = (runtime) => {
  runtime.credentialsIssuer.signIn({ providerUserId: "x" });
  return { routeHandler: () => new Response() };
};
const loginParts: Awaited<ReturnType<ServerAuthAdapterFor<typeof loginAuth>>> =
  {
    // @ts-expect-error a login handler implements no AuthHandler: Wasp recognises its own credential
    handler,
    routeHandler: () => new Response(),
  };
void loginParts;
declare function credentialAuth(): { kind: "credential"; server: {} };
export const credentialTyped: ServerAuthAdapterFor<
  typeof credentialAuth
> = () => ({ handler });
// @ts-expect-error a credential handler must return its AuthHandler
const credentialWithout: Awaited<
  ReturnType<ServerAuthAdapterFor<typeof credentialAuth>>
> = {};
void credentialWithout;
