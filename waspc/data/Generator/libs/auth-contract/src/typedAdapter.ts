/**
 * Typed adapters: the types an adapter receives, derived from the manifest
 * its handler's spec helper returns. Types only; nothing here exists at
 * runtime.
 */

import type {
  Credentials,
  RuntimeGrantName,
  ServerAuthHandlerParts,
  WaspEmail,
  WaspServerRuntime,
} from "./index.js";

/**
 * `ServerAuthAdapter`, with everything the adapter receives typed FROM THE
 * HANDLER'S SPEC HELPER, so a package author states each fact once (in the
 * manifest the helper returns) and gets it back as types:
 *
 *   // spec.ts
 *   export function myAuth(props: MyAuthProps) { return { server: {...}, ... } }
 *   // server.ts
 *   export const createServerAuthHandler: ServerAuthAdapterFor<typeof myAuth> =
 *     (runtime, spec) => { ... }
 *
 * What is read off the helper's RETURN TYPE, and what it types:
 *
 *   manifest field          types
 *   ----------------------  ------------------------------------------------
 *   `server.spec`           the `spec` parameter
 *   `identityNamespaces`    the keys of `runtime.identities`
 *   `server.env`            the keys of `runtime.env`
 *   `credentials`           whether `runtime.credentials` is there
 *   `uses`                  whether `runtime.email` is there
 *   `server.routes`         whether the adapter must return a `routeHandler`
 *
 * REFERENCES. The helper types a field that takes app code as
 * `SpecReference<AppValue>` (`configFn?: SpecReference<OAuthConfigFn>`), and
 * the adapter receives `AppValue` in that place: `spec` is typed
 * `LiveSpec<...>` of the manifest's `server.spec`. So the spec's shape is
 * written ONCE, in the helper, instead of once with references and once with
 * live functions. It does not check the app's function: in `main.wasp.ts` a
 * `with { type: "ref" }` import is a ref object, whatever it points at.
 *
 * THREE STATES, NOT A FLAG. A facet is `credentials` or `email`:
 * - the manifest field is REQUIRED in the helper's return type: the facet is
 *   always there; use it directly.
 * - the field is OPTIONAL (`credentials?:`, or `uses: Array<"email-send">`,
 *   which may be empty): the APP decides. The facet is reachable only after
 *   checking `runtime.hasCredentials` / `runtime.canSendEmail`, which narrows.
 * - the field is absent: the facet is not on the type at all.
 *
 * Why this is sound where a hand-set type flag was not: the helper's body is
 * checked against its own return type, so a helper that promises
 * `credentials` must actually return one. The types hide what would reject;
 * at runtime every facet is still a member (see `WaspServerRuntime`).
 *
 * What stays unchecked: that the adapter an app wires up belongs to this
 * helper. A package's helper names its own adapter, so that holds by
 * construction; a hand-written handler uses the loose `ServerAuthAdapter`.
 *
 * `SpecHelper` is the helper's type (`typeof myAuth`), or a manifest type.
 * The helper must not be generic: a type parameter would be inferred as
 * `unknown`.
 */
export type ServerAuthAdapterFor<SpecHelper> = (
  runtime: WaspServerRuntimeFor<SpecHelper>,
  spec: ServerSpecOf<SpecHelper>,
) =>
  | ServerAuthHandlerPartsFor<SpecHelper>
  | Promise<ServerAuthHandlerPartsFor<SpecHelper>>;

/** The manifest type a spec helper returns (or the type itself, when given a manifest). */
export type ManifestOf<SpecHelper> = SpecHelper extends (
  ...args: never[]
) => infer Manifest
  ? Manifest
  : SpecHelper;

/** The manifest's `server.spec` with its references live: the type of the server adapter's `spec` parameter. */
export type ServerSpecOf<SpecHelper> =
  ManifestOf<SpecHelper> extends { server: { spec?: infer ServerSpec } }
    ? LiveSpec<ServerSpec>
    : unknown;

/** The manifest's `identityNamespaces` as a union of suffixes; `"default"` when it declares none. */
export type IdentityNamespacesOf<SpecHelper> =
  ManifestOf<SpecHelper> extends {
    identityNamespaces: ReadonlyArray<infer Namespace extends string>;
  }
    ? Namespace
    : "default";

/**
 * `WaspServerRuntime` as one particular handler sees it: the same members,
 * with `identities`, `env` and the two facets narrowed to what the manifest
 * declares.
 */
export type WaspServerRuntimeFor<SpecHelper> = Omit<
  WaspServerRuntime<IdentityNamespacesOf<SpecHelper>>,
  "env" | "credentials" | "hasCredentials" | "email" | "canSendEmail"
> & {
  /**
   * One key per env var in the manifest's `server.env`. An undeclared name is
   * a type error. A required var is a plain `string` when `env` is typed as a
   * tuple (`[{ name: "CLERK_SECRET_KEY" }]`); with an array type the list
   * itself may vary per app, so every value is `string | undefined`.
   */
  env: DeclaredEnv<
    ManifestOf<SpecHelper> extends { server: { env: infer EnvVars } }
      ? EnvVars
      : []
  >;
} & Facet<
    CredentialsDeclaration<ManifestOf<SpecHelper>>,
    "hasCredentials",
    "credentials",
    Credentials
  > &
  Facet<
    GrantDeclaration<ManifestOf<SpecHelper>, "email-send">,
    "canSendEmail",
    "email",
    WaspEmail
  >;

/** What the adapter must return: a `routeHandler` exactly when the manifest declares `server.routes`. */
export type ServerAuthHandlerPartsFor<SpecHelper> = Pick<
  ServerAuthHandlerParts,
  "handler"
> &
  (ManifestOf<SpecHelper> extends { server: { routes: object } }
    ? Required<Pick<ServerAuthHandlerParts, "routeHandler">>
    : ManifestOf<SpecHelper> extends { server: { routes?: object } }
      ? Pick<ServerAuthHandlerParts, "routeHandler">
      : {
          /** The manifest declares no `routes`, so Wasp would never mount it. */ routeHandler?: never;
        });

/**
 * A reference to the app's code, as a spec helper types it:
 * `configFn?: SpecReference<OAuthConfigFn>`. In `main.wasp.ts` the app passes a
 * `with { type: "ref" }` import there. Such an import is a ref OBJECT (Wasp
 * turns it into the real value only in the generated code), so `AppValue`
 * cannot check the app's function. What it does is say what the adapter
 * receives in that place: see `LiveSpec`.
 *
 * Deliberately structural, and not `Reference` from `@wasp.sh/spec`: that one
 * is branded per copy of the package, and a handler package's copy is not the
 * app's.
 */
export type SpecReference<AppValue> = {
  readonly kind: "refObject";
  /** Never set. Carries `AppValue` so `LiveSpec` can recover it. */
  readonly __appValue?: AppValue;
};

/**
 * A spec as the ADAPTER receives it: the same object, with every
 * `SpecReference<AppValue>` replaced by its `AppValue`, at any depth. Wasp does
 * exactly that at runtime (it lifts the references out, carries the rest
 * across the compiler as JSON, and sets the live values back).
 */
export type LiveSpec<Spec> =
  Spec extends SpecReference<infer AppValue>
    ? AppValue
    : Spec extends ((...args: never[]) => unknown) | ReadonlyArray<unknown>
      ? Spec
      : Spec extends object
        ? { [Key in keyof Spec]: LiveSpec<Spec[Key]> }
        : Spec;

/** How firmly a manifest declares something: in every app, in some apps, or in none. */
export type Declaration = "always" | "app-decides" | "never";

/** Read off the `credentials` field: required, optional or absent. */
export type CredentialsDeclaration<Manifest> =
  "credentials" extends keyof Manifest
    ? {} extends Pick<Manifest, "credentials" & keyof Manifest>
      ? "app-decides"
      : "always"
    : "never";

/** Read off `uses`: a tuple naming the grant, an array that may name it, or neither. */
export type GrantDeclaration<
  Manifest,
  Grant extends RuntimeGrantName,
> = Manifest extends {
  uses: infer Uses extends ReadonlyArray<unknown>;
}
  ? number extends Uses["length"]
    ? Grant extends Uses[number]
      ? "app-decides"
      : "never"
    : Grant extends Uses[number]
      ? "always"
      : "never"
  : "never";

/** A runtime facet and its flag, present according to its `Declaration`. */
export type Facet<
  Declared extends Declaration,
  Flag extends string,
  Name extends string,
  FacetType,
> = Declared extends "always"
  ? { [F in Flag]: true } & { [N in Name]: FacetType }
  : Declared extends "never"
    ? { [F in Flag]: false }
    :
        | ({ [F in Flag]: true } & { [N in Name]: FacetType })
        | { [F in Flag]: false };

/** `runtime.env` for a declared list of env vars. */
export type DeclaredEnv<EnvVars> =
  EnvVars extends ReadonlyArray<infer EnvVar>
    ? {
        readonly [Var in EnvVar as Var extends {
          name: infer Name extends string;
        }
          ? Name
          : never]: number extends EnvVars["length"]
          ? string | undefined
          : Var extends { name: infer Name; optional?: false }
            ? string extends Name
              ? string | undefined
              : string
            : string | undefined;
      }
    : {};
