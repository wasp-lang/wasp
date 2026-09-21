/**
 * Type-level tests for the typed adapters. Checked by `npm test` (`tsc`);
 * every `@ts-expect-error` is an assertion that the line does not compile.
 */

import type { ClientAuthAdapterFor } from "../client.js";
import type {
  AuthHandler,
  ServerAuthAdapterFor,
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
  identityNamespaces: Array<"email" | "google">;
  credentials: { transport?: "bearer" | "cookie" };
};
export const wasp: ServerAuthAdapterFor<typeof waspAuthT> = (runtime, spec) => {
  runtime.credentialsIssuer.signIn({ subjectId: "a" });
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
  // @ts-expect-error undeclared namespace
  runtime.identities.github;
  return { handler, routeHandler() {} };
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
  routeHandler() {},
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
  return { handler, routeHandler() {} };
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
