/**
 * Typed adapters: the types an adapter receives, derived from the manifest
 * its handler's spec constructor returns. Types only; nothing here exists at
 * runtime.
 */

import type {
  CredentialsIssuer,
  IdentityStore,
  ProviderKind,
  ProviderKindParam,
  RuntimeGrantName,
  ServerCredentialAuthHandlerParts,
  ServerLoginAuthHandlerParts,
  WaspEmail,
  WaspServerRuntime,
} from "./index.js";

/**
 * `ServerAuthAdapter`, with everything the adapter receives typed FROM THE
 * HANDLER'S SPEC HELPER, so a package author states each fact once (in the
 * manifest the constructor returns) and gets it back as types:
 *
 *   // spec.ts
 *   export function myAuth(props: MyAuthProps) { return { server: {...}, ... } }
 *   // server.ts
 *   export const createServerAuthHandler: ServerAuthAdapterFor<typeof myAuth> =
 *     (runtime, spec) => { ... }
 *
 * What is read off the constructor's RETURN TYPE, and what it types:
 *
 *   manifest field          types
 *   ----------------------  ------------------------------------------------
 *   `server.spec`           the `spec` parameter
 *   `providers`        the keys of `runtime.identities`, and by their `kind`
 *                           what each store's calls must carry
 *   `server.env`            the keys of `runtime.env`
 *   `credentials`           whether `runtime.credentialsIssuer` is there
 *   `uses`                  whether `runtime.email` is there
 *   `server.routes`         whether the adapter must return a `routeHandler`
 *
 * REFERENCES. The constructor types a field that takes app code as
 * `SpecReference<AppValue>` (`configFn?: SpecReference<OAuthConfigFn>`), and
 * the adapter receives `AppValue` in that place: `spec` is typed
 * `LiveSpec<...>` of the manifest's `server.spec`. So the spec's shape is
 * written ONCE, in the constructor, instead of once with references and once with
 * live functions. It does not check the app's function: in `main.wasp.ts` a
 * `with { type: "ref" }` import is a ref object, whatever it points at.
 *
 * THREE STATES, NOT A FLAG. A facet is `credentialsIssuer` or `email`:
 * - the manifest field is REQUIRED in the constructor's return type: the facet is
 *   always there; use it directly.
 * - the field is OPTIONAL (`credentials?:`, or `uses: Array<"email-send">`,
 *   which may be empty): the APP decides. The facet is reachable only after
 *   checking `runtime.hasCredentialsIssuer` / `runtime.canSendEmail`, which narrows.
 * - the field is absent: the facet is not on the type at all.
 *
 * Why this is sound where a hand-set type flag was not: the constructor's body is
 * checked against its own return type, so a constructor that promises
 * `credentials` must actually return one. The types hide what would reject;
 * at runtime every facet is still a member (see `WaspServerRuntime`).
 *
 * What stays unchecked: that the adapter an app wires up belongs to this
 * constructor. A package's constructor names its own adapter, so that holds by
 * construction; a hand-written handler uses the loose `ServerLoginAuthAdapter`
 * or `ServerCredentialAuthAdapter`.
 *
 * `SpecConstructor` is the constructor's type (`typeof myAuth`), or a manifest type.
 * The constructor must not be generic: a type parameter would be inferred as
 * `unknown`.
 */
export type ServerAuthAdapterFor<SpecConstructor> = (
  runtime: WaspServerRuntimeFor<SpecConstructor>,
  spec: ServerSpecOf<SpecConstructor>,
) =>
  | ServerAuthHandlerPartsFor<SpecConstructor>
  | Promise<ServerAuthHandlerPartsFor<SpecConstructor>>;

/** The manifest type a spec constructor returns (or the type itself, when given a manifest). */
export type ManifestOf<SpecConstructor> = SpecConstructor extends (
  ...args: never[]
) => infer Manifest
  ? Manifest
  : SpecConstructor;

/** The manifest's `server.spec` with its references live: the type of the server adapter's `spec` parameter. */
export type ServerSpecOf<SpecConstructor> =
  ManifestOf<SpecConstructor> extends { server: { spec?: infer ServerSpec } }
    ? LiveSpec<ServerSpec>
    : unknown;

/** The kind of every provider the manifest declares, by name; just `default` when it declares none. */
export type ProviderKindsOf<SpecConstructor> =
  ManifestOf<SpecConstructor> extends { providers: infer Providers }
    ? {
        [ProviderName in keyof Providers & string]-?: KindOf<
          NonNullable<Providers[ProviderName]>
        >;
      }
    : { default: undefined };

/** One declaration's kind: the literal when it is fixed, both when the app decides, `undefined` when it has none. */
export type KindOf<Declaration> = Declaration extends {
  kind: infer Kind extends ProviderKind;
}
  ? Kind
  : "kind" extends keyof Declaration
    ? ProviderKindParam
    : undefined;

/** The manifest's provider names as a union; `"default"` when it declares none. */
export type ProviderNamesOf<SpecConstructor> =
  keyof ProviderKindsOf<SpecConstructor> & string;

/**
 * `WaspServerRuntime` as one particular handler sees it: the same members,
 * with `identities`, `env` and the two facets narrowed to what the manifest
 * declares.
 */
export type WaspServerRuntimeFor<SpecConstructor> = Omit<
  WaspServerRuntime<ProviderNamesOf<SpecConstructor>>,
  | "env"
  | "identities"
  | "credentialsIssuer"
  | "hasCredentialsIssuer"
  | "email"
  | "canSendEmail"
> & {
  /**
   * One store per declared provider, each typed by its kind: an "oauth"
   * provider's `create`, `provision` and `link` REQUIRE the `oauth` data, a
   * provider without a kind does not accept it.
   */
  identities: {
    readonly [ProviderName in ProviderNamesOf<SpecConstructor>]: IdentityStore<
      Extract<ProviderKindsOf<SpecConstructor>[ProviderName], ProviderKindParam>
    >;
  };
  /**
   * One key per env var in the manifest's `server.env`. An undeclared name is
   * a type error. A required var is a plain `string` when `env` is typed as a
   * tuple (`[{ name: "CLERK_SECRET_KEY" }]`); with an array type the list
   * itself may vary per app, so every value is `string | undefined`.
   */
  env: DeclaredEnv<
    ManifestOf<SpecConstructor> extends { server: { env: infer EnvVars } }
      ? EnvVars
      : []
  >;
} & Facet<
    CredentialsDeclaration<ManifestOf<SpecConstructor>>,
    "hasCredentialsIssuer",
    "credentialsIssuer",
    CredentialsIssuer<ProviderKindsOf<SpecConstructor>>
  > &
  Facet<
    GrantDeclaration<ManifestOf<SpecConstructor>, "email-send">,
    "canSendEmail",
    "email",
    WaspEmail
  >;

/**
 * What the adapter must return, by the manifest's `kind`: a login handler
 * returns routes only; a credential handler returns its `AuthHandler`, plus a
 * `routeHandler` exactly when the manifest declares `server.routes`.
 */
export type ServerAuthHandlerPartsFor<SpecConstructor> =
  ManifestOf<SpecConstructor> extends { kind: "login" }
    ? ServerLoginAuthHandlerParts
    : Pick<ServerCredentialAuthHandlerParts, "handler"> &
        (ManifestOf<SpecConstructor> extends { server: { routes: object } }
          ? Required<Pick<ServerCredentialAuthHandlerParts, "routeHandler">>
          : ManifestOf<SpecConstructor> extends { server: { routes?: object } }
            ? Pick<ServerCredentialAuthHandlerParts, "routeHandler">
            : {
                /** The manifest declares no `routes`, so Wasp would never mount it. */ routeHandler?: never;
              });

/**
 * A reference to the app's code, as a spec constructor types it:
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
