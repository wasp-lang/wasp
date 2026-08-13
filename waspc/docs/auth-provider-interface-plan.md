# Auth provider interface: the implementation plan

Option (2) from the discussion — define an interface for "Auth", let external solutions
implement it via adapters. This is the plan, plus the reasoning behind the API choices.

Design informed by a four-way independent design panel and by the prior-art research in
`auth-option-2-provider-interface.md`. Where the panel disagreed, the disagreement is recorded
rather than hidden.

---

## 1. Where the uniform line goes

**The rule: Wasp makes uniform everything that answers _"who is the current user, and what may
they do"_. Wasp does not make uniform anything that answers _"how does someone become the
current user"_.**

Reading an identity is uniform. Establishing one is not.

That is not an arbitrary split — it is forced by the providers themselves. Clerk has no
server-side password login at all, so a uniform `login(email, password)` could only be
implemented for Clerk as something that throws at runtime or silently ignores its arguments.
Both are lies discovered in production; a missing export is discovered at `wasp start`.

| Item                                                                                    | Class                 | Notes                                                                                                                                                                      |
| --------------------------------------------------------------------------------------- | --------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `context.user`                                                                          | **UNIFORM**           | Always a row from the developer's `User` table. The load-bearing guarantee                                                                                                 |
| `useAuth()`, `getMe`                                                                    | **UNIFORM**           |                                                                                                                                                                            |
| `req.user`, `req.sessionId`                                                             | **UNIFORM**           |                                                                                                                                                                            |
| `authRequired` pages, `auth: true` operations/APIs/CRUD                                 | **UNIFORM**           | Wasp owns routing and middleware; no provider is involved                                                                                                                  |
| `logout()`                                                                              | **UNIFORM** signature | See the caveat below                                                                                                                                                       |
| `AuthUser` (developer's fields)                                                         | **UNIFORM**           |                                                                                                                                                                            |
| `userSignupFields`, `defineUserSignupFields`                                            | **UNIFORM** mechanism | Becomes _the_ way the `User` row is populated, so it grows in importance                                                                                                   |
| `onBeforeSignup`, `onAfterSignup`                                                       | **UNIFORM**           | Because Wasp owns provisioning, these fire even for providers that have no blocking hook of their own. Wasp _gains_ a signup veto on Clerk                                 |
| `AuthUser.identities`                                                                   | **TIERED**            | Key set is generated per provider. Swapping providers is a compile error at each site that reads a provider-specific identity — which is the correct feedback              |
| `getEmail`, `getUsername`                                                               | **TIERED**            | Work under wasp-auth. The portable pattern is `user.email` on the developer's own entity, populated by `userSignupFields` — which is what Django, Rails and Payload all do |
| `onBeforeLogin`, `onAfterLogin`                                                         | **TIERED**            | Only providers with an observable login moment can fire them                                                                                                               |
| `onAfterEmailVerified`, `onBeforeOAuthRedirect`                                         | **PROVIDER-SPECIFIC** | Hooks into flows the interface does not contain                                                                                                                            |
| `login`, `signup`                                                                       | **PROVIDER-SPECIFIC** | Same import path, different symbols per provider                                                                                                                           |
| `LoginForm` and the other auth forms                                                    | **PROVIDER-SPECIFIC** | Clerk ships `<SignIn/>`; wasp-auth ships today's forms unchanged                                                                                                           |
| `methods: { email \| usernameAndPassword \| google \| ... }`                            | **PROVIDER-SPECIFIC** | Only meaningful under the `wasp` provider                                                                                                                                  |
| `createUser`, `findAuthIdentity`, `sanitizeAndSerializeProviderData`, `getProviderData` | **PROVIDER-SPECIFIC** | wasp-auth internals that leaked into `wasp/server/auth`; re-exported from the same path while wasp-auth is selected                                                        |

**Why this is a good deal even though login isn't portable.** The auth pages are 2–5 files. The
rest of the app is hundreds, and 100% of it — every operation, every query, every relation,
every `context.user` — is portable.

### The one caveat that must be enforced, not documented

`logout()` keeps a uniform signature, but its _guarantee_ is tiered: clearing the client
credential does not kill a session the provider holds in a cookie on its own domain.

So: **a provider declaring `credentialTransport: 'cookie'` MUST also declare
`sessionRevocation`, rejected at boot if it doesn't.** All four target providers have
revocation APIs, so this costs nothing today and closes a security-relevant footgun tomorrow.

---

## 2. The interface

Two principles, both learned the hard way by other frameworks:

**Verify, don't fetch.** AdonisJS shapes its provider around `findById(id)`, which assumes
identity is retrievable by an opaque id. External providers validate a signed credential and
cannot look a subject up on your behalf. So the primitive is `verify`.

**Base interface plus capability extensions, not one interface with optional members.**
RedwoodJS modelled every optional capability as a separate generic and ended up with **13 type
parameters** on its client interface. Named extensions keep the base readable and make
capability a first-class, detectable thing.

**And the reason Wasp can get away with a small type surface at all is worth stating, because
it is the sharpest thing the design panel produced:**

> Wasp ships a **compiler**, not a library. Redwood had to express capability variation _in the
> type system_ because a library must typecheck for every configuration at once. Wasp expresses
> it by **generating different code**. Capability variation therefore lives in the generator,
> and the type system only ever has to describe the one concrete provider that was selected.

That collapses most of the difficulty. It also means the eventual shape may be better as a
single `AuthProvider<C extends Capability>` with the optional half derived by
`Pick<CapabilityPorts, C>` — one type parameter for the capability _set_ — rather than a
hand-written interface per subset, which works for two tiers and collapses at five. PR 1 uses
the simple two-interface form because there are exactly two tiers today; revisit at PR 5.

```ts
export type VerifiedSession = {
  sessionId: string; // opaque, provider-owned; used for logout + websocket auth only
  subjectId: string; // the provider's stable id for the subject
};

export interface AuthProvider {
  readonly id: string;
  verifyRequest(req: ExpressRequest): Promise<VerifiedSession | null>; // null = unauthenticated, not an error
  verifyCredential(credential: string): Promise<VerifiedSession | null>; // websockets carry a bare token
  revokeSession(sessionId: string): Promise<void>;
}

/** Providers that can mint sessions server-side. Clerk cannot. */
export interface SessionIssuingAuthProvider extends AuthProvider {
  issueSession(subjectId: string): Promise<VerifiedSession>;
  revokeAllSessions(subjectId: string): Promise<void>;
}

export function canIssueSessions(
  p: AuthProvider,
): p is SessionIssuingAuthProvider;
```

### Compile-time capability detection — Wasp can do what Redwood couldn't

`main.wasp.ts` is **executed with `node`** and its result is written to `spec-result.json`,
which the Haskell compiler reads (`Wasp/Project/WaspFile/TypeScript.hs:98`). So a provider
declaration is ordinary compile-time data:

```ts
auth: {
  userEntity: "User",
  provider: waspAuth({ methods: { email: { ... } } }),
}
```

`waspAuth(...)` returns a manifest — `{ id, capabilities, identityKinds, config }` — that the
generator branches on. A user who selects Clerk and imports `LoginForm` gets a build error,
because the generator never emitted it. **This is the mechanism Redwood lacked, and it is why
Wasp can tier capabilities safely where Redwood could only document them.**

---

## 3. The invariant that prevents the Redwood failure

RedwoodJS shipped nine adapters over one interface, unified authentication, and did **not**
unify provisioning. Result: `currentUser.id` is a Postgres `Int` under dbAuth and a
`"user_2abc…"` string under Clerk, and every relation in the app has to know which.

**Wasp's invariant: `context.user` is always a row in the developer's `User` table, for every
provider, with no exceptions.** That makes just-in-time provisioning mandatory rather than
optional:

```
verify(request) -> { sessionId, subjectId }
  -> find local user for (providerId, subjectId)
       found     -> return it
       not found -> provision, in ONE transaction:
                      onBeforeSignup (can veto)
                      userSignupFields(claims) -> create User
                      link the identity
                      onAfterSignup (after commit)
```

Three details that are not optional, all taken from systems that got this right:

- **`get_or_create` semantics, not try/except** (Django says so in a source comment). N parallel
  first requests must produce exactly one `User`.
- **Provision in-band, at login — never by webhook.** Clerk's own sync-data guide stops at a
  `console.log`. Webhooks are for deletion and drift only.
- **Idempotent, with a `created` flag** so the same hook can safely run on every login
  (Django's `configure_user(request, user, created)`; Better Auth's `provisionUserOnEveryLogin`).

---

## 4. The stacked PRs

The single most useful structural idea from the panel: **put a stop-gate in the middle.**

Everything before it is a pure internal refactor that is independently valuable — it is
essentially what issues #3081 and #4677 already ask for. Everything after it is a one-way door.
So the team can build the refactor now and defer the actual bet on option (2).

**Acceptance criterion for every PR before the gate: the e2e goldens are byte-identical.**
That is the strongest possible no-change proof, because the generated output is compared
verbatim.

| PR    | What                                                                                                                                                                                 | User-visible?                                          |
| ----- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | ------------------------------------------------------ |
| **1** | **Extract `AuthProvider`; implement it with today's Lucia code; stop leaking Lucia's `Session` type.** ✅ _done, verified_                                                           | No                                                     |
| 2     | Collapse `verifyRequest`/`verifyCredential` into one `authenticate(Request)`; websocket synthesises a `Request` from its handshake                                                   | Wire change for websockets, one release of back-compat |
| 3     | Route all five session-creating call sites through one core `resolveUser` function                                                                                                   | No                                                     |
| 4     | Normalise claims; rewire `getEmail`/`getUsername` to read them with fallback to today's behaviour                                                                                    | No                                                     |
| 5     | `AuthProvider` contract published **in `@wasp.sh/lib-auth`** (see note); compiler synthesises `waspAuth({ methods: <existing app.auth.methods> })` so `main.wasp.ts` does not change | No                                                     |
| 6     | Core-owned provisioning; wasp-auth's signup paths call `provisionUser`. Concurrency test: N parallel first-requests → 1 user, 1 `onAfterSignup`                                      | No                                                     |
| —     | **← STOP-GATE.** Everything above ships as internal improvement. If no second provider ever materialises, nothing is wasted                                                          |                                                        |
| 7     | `app.auth.provider` in AppSpec; capability-gated codegen; golden fixture with a zero-capability stub asserting `login`/`LoginForm` are omitted                                       | Additive config                                        |
| 8     | Split the barrels; deprecate `identities`/`getEmail` with a `user.email` migration doc                                                                                               | **Yes**                                                |
| 9     | Clerk adapter — the hardest case: no password login, no signup, no redirect hook, cookie transport                                                                                   | Additive                                               |
| 10    | Better Auth adapter — proves mounted routes and a provider that owns its own storage                                                                                                 | Additive                                               |

---

### Two corrections from reading the source

**The contract must eventually live in `@wasp.sh/lib-auth`, not in generated SDK templates.**
That package already ships to npm. A third-party adapter cannot import from code Wasp generates
into someone's project, so anything a community adapter needs to implement has to be a normal
npm dependency. PR 1 puts the interface in the SDK because there is only one implementation and
it is internal; PR 5 is where it has to move out.

**Tiering `AuthUser.identities` needs no new machinery.** Its keys are _already_ generated per
enabled method via mustache conditionals in `sdk/wasp/auth/user.ts`. Generating them from a
provider manifest instead is the same mechanism pointed at a different input.

## 5. PR 1, as built and verified

**What changed**

- New `sdk/wasp/server/auth/provider/{types,wasp,index}.ts` — the interface, a Lucia-backed
  implementation of it, and the single place a provider is selected.
- `session.ts` is now a thin façade over `authProvider`. It no longer imports Lucia and no
  longer leaks Lucia's `Session` type; it returns `{ sessionId, user }`.
- `core/auth.ts` reads `sessionAndUser.sessionId`.

**Lucia is now confined to exactly two files** (`lucia.ts` and `provider/wasp.ts`). Nothing in
the middleware, the websocket handler, the logout route or the login paths references it.

**Behaviour is preserved.** Verified live against a running app:

```
POST /auth/email/signup                       200  {"success":true}
POST /auth/email/login                        200  {"sessionId":"cb6tlye7…"}
GET  /auth/me      (Bearer)                   200  {"id":"7de15c67-…","username":"alice_wonder",
                                                    "identities":{"email":{"isEmailVerified":true,…}}}
POST /auth/email/login  (wrong password)      401
POST /auth/logout                             200  {"success":true}
GET  /auth/me      (after logout)             401
GET  /auth/me      (no token)                 200  {"json":null}
```

**One subtlety worth reviewing.** The old code got `Auth.userId` for free out of Lucia's
`validateSession` (via `getUserAttributes`) and then loaded the user by that id. Resolving
through the provider interface loses that free ride, so a naive port would add a query per
authenticated request. Instead `toSessionAndUser` loads the user _through_ the auth relation in
a single query:

```ts
prisma.user.findFirst({
  where: { auth: { id: authId } },
  include: { auth: { include: { identities: true } } },
});
```

Same query count as before, and it happens to be the shape every future provider needs anyway.

**Known break:** `wasp/server/auth/session` is a public entrypoint and
`getSessionAndUserFrom*` now returns `{ sessionId, user }` rather than `{ session, user }`.
Everything in that module is marked `PRIVATE API` and only `.id` was ever read, but it is a
change to an exported signature and should be called out in release notes.

---

## 5b. Four refinements from the judging round

**1. Capability detection on the client should be _module resolution_, not types.** The
generated barrel does `export * from '<selected provider client module>'`. Then "does this
provider have `LoginForm`?" is answered by `tsc` at the exact import site, with zero generics
and zero optional members. This is strictly better than a capabilities manifest for the client
surface, and it is a move only a compiler-backed framework can make.

**2. Capabilities must be an open set, not a closed record.** If `AuthCapabilities` is a fixed
record of booleans mirrored into a Haskell record with derived `FromJSON`, then adding a sixth
capability breaks decoding of every manifest emitted by an adapter built against an older
package — and a third party can never introduce one at all. Model it as a list of strings with
unknown entries ignored.

**3. The subject must stay opaque — which PR 1 gets right, and it matters more than it looks.**
Wasp's sessions are keyed on `Auth.id`, not on the developer's `User.id`. A port designed around
`issueSession(userId)` forces the wasp-auth adapter to do the `Auth → User` join itself
(violating "the provider never touches the DB") or forces core into an extra lookup per request.
`VerifiedSession.subjectId` is deliberately opaque and core resolves it, which is what a
session-to-`Auth` model actually wants. This mismatch would not have bitten any hosted provider
— only the one adapter the whole migration is built around.

**4. The `claims` channel is the weakest link and needs specifying before PR 6.** JIT
provisioning is only as good as the claims that feed `userSignupFields`. Concretely: **Clerk's
default session claims contain no email**, so if the developer's `User.email` is non-nullable,
the first request from a new user throws a Prisma validation error and that account is
permanently 401'd. Make `claims` a typed, documented contract; state per-adapter which fields
are populated; and decide the drift story (re-sync on every login, à la
`provisionUserOnEveryLogin`) rather than leaving it as a suggestion.

## 6. Open questions the panel split on

**Do `identities` and `getEmail` survive at all?** One designer argued they should be deleted
outright — a Wasp-defined shape whose contents depend on the installed provider is the Redwood
mistake one level down, and `getEmail` returning `null` for half of all providers is a footgun
in a password-reset flow. The other kept them tiered, on the grounds that swapping providers is
a data migration anyway and a compile error at each read site is the right feedback.

Both agree on the _destination_: email belongs on the developer's `User` row, put there by
`userSignupFields`. They differ on whether to remove the old accessors now or deprecate them.
**Plan above takes the gentler path** (tiered + deprecate at PR 8) — but this is a real choice,
not a settled one.

**Do login hooks get faked for stateless providers?** With a JWT provider there is no observable
login: request 1 and request 500 look identical. One designer proposed a session ledger; another
rejected it as duplicating state Wasp does not own.

**I initially sided against the ledger and the panel changed my mind**, because the good version
of it is cheaper than it sounds: reuse the **existing `Session` table** as a witness row, insert
`claims.sessionId` with `ON CONFLICT DO NOTHING`, and _"the insert succeeded"_ **is** the
first-sighting-of-a-login event. One write per real login, no new table, no state Wasp has to
keep in sync. It makes `onBeforeLogin`/`onAfterLogin` **and** server-side `logout` implementable
for providers that are stateless from Wasp's point of view. Worth doing — but after the
stop-gate, not before.

⚠️ **The trap that comes with it:** under a hosted provider, a thrown `onBeforeSignup` **loops
forever**. The veto blocks the local `User` row, but the upstream session survives, so the next
request re-triggers provisioning, which re-throws. Any implementation of core-owned provisioning
needs a rejection cache or a persisted tombstone. This is a real bug class, not a hypothetical.

**The counter-intuitive risk, and the one most worth internalising.** This interface is
_easiest_ for Clerk — no schema, no routes, pure verification — and _hardest_ for **Better
Auth**, which is the provider actually motivating the work. A provider that owns its own storage
needs `ownRoutes` (a raw Express `Router`) and its own Prisma client, which drags in a second
schema, a second `prisma generate`, two connection pools, and a `wasp db migrate` story that
nobody has costed. So the design is strongest against the easy case and weakest against the one
on the roadmap. **Prototype the Better Auth adapter before committing past the stop-gate**, not
the Clerk one.

**Reuse `AuthIdentity` for the provider linkage, or add a table?** `AuthIdentity` currently
means "which auth _method_" (email/username/google). Provider linkage is "which auth
_provider_". Overloading one table for both conflates two concepts; a separate additive table
avoids touching the existing schema and can be backfilled. Leaning separate, not decided.
