> Fetch the complete documentation index at: https://wasp.sh/llms.txt
---

# @wasp/spec

## Wasp Spec

### App

Root shape of a Wasp app specification.

Pass an `App` to the [app](#app-1) constructor and `export default` the result from `main.wasp.ts`.

#### Example

```ts
import { app } from "@wasp/spec"

export default app({
  name: "todoApp",
  wasp: { version: "^0.24.0" },
  title: "ToDo App",
  head: ["<link rel='icon' href='/favicon.ico' />"],
  spec: [],
});
```

#### Properties

##### auth? {#auth-2}

> `optional` **auth?**: [`Auth`](#auth-3)

Configuration for authentication. Enables auth when set.

##### client?

> `optional` **client?**: [`Client`](#client-1)

Configuration for the client part of the resulting Wasp app.

##### db?

> `optional` **db?**: [`Db`](#db-1)

Configuration for the app's database.

##### deployment?

> `optional` **deployment?**: [`Deployment`](#deployment-1)

Configuration for how the app will be deployed.

##### emailSender?

> `optional` **emailSender?**: [`EmailSender`](#emailsender-1)

Configuration for the app's email sender.

##### head?

> `optional` **head?**: `string`\[]

Extra tags injected into the HTML `<head>`.

Each entry is rendered inside a React component, so the strings must be valid JSX: self-closing tags must end with `/>` (e.g. `<meta ... />`), and attributes must be camelCased (e.g. `httpEquiv` instead of `http-equiv`).

Due to a [React bug](https://github.com/facebook/react/issues/36169), avoid `defer` on `<script>` tags because it can cause hydration warnings. Use `async` instead.

##### name

> **name**: `string`

Internal app name.

Must not contain spaces.

##### server?

> `optional` **server?**: [`Server`](#server-1)

Configuration for the server part of the resulting Wasp app.

##### spec

> **spec**: [`Spec`](#spec-1)

The specification ([Spec](#spec-1)) of the app.

Build entries with the dedicated constructors ([page](#page-2), [route](#route-1), [query](#query-1), [action](#action-1), [api](#api-1), [apiNamespace](#apinamespace-1), [job](#job-1), [crud](#crud-1)).

##### title

> **title**: `string`

App title.

Used as the browser tab title.

##### wasp

> **wasp**: [`Wasp`](#wasp-1)

Wasp metadata.

This will get checked against the Wasp CLI.

##### webSocket?

> `optional` **webSocket?**: [`WebSocket`](#websocket-1)

Configuration for the app's WebSocket support.

---

### AppConfig

> **AppConfig** = `Omit`<[`App`](#app), `"kind"`>

The configuration object accepted by the [app](#app-1) constructor.

---

### app()

> **app**(`config`): [`App`](#app)

Creates a Wasp [App](#app).

Call `app()` exactly once in your `main.wasp.ts` and export the result as the file's default export. The Wasp compiler reads this default export to generate your app.

See the [Wasp Spec docs](https://wasp.sh/docs/features/spec) for the full shape of the configuration.

#### Parameters

##### config

The app configuration.

###### auth? {#auth-5}

[`Auth`](#auth-3)

Configuration for authentication. Enables auth when set.

###### client? {#client-2}

[`Client`](#client-1)

Configuration for the client part of the resulting Wasp app.

###### db? {#db-2}

[`Db`](#db-1)

Configuration for the app's database.

###### deployment? {#deployment-2}

[`Deployment`](#deployment-1)

Configuration for how the app will be deployed.

###### emailSender? {#emailsender-2}

[`EmailSender`](#emailsender-1)

Configuration for the app's email sender.

###### head?

`string`\[]

Extra tags injected into the HTML `<head>`.

Each entry is rendered inside a React component, so the strings must be valid JSX: self-closing tags must end with `/>` (e.g. `<meta ... />`), and attributes must be camelCased (e.g. `httpEquiv` instead of `http-equiv`).

Due to a [React bug](https://github.com/facebook/react/issues/36169), avoid `defer` on `<script>` tags because it can cause hydration warnings. Use `async` instead.

###### name {#name-4}

`string`

Internal app name.

Must not contain spaces.

###### server? {#server-2}

[`Server`](#server-1)

Configuration for the server part of the resulting Wasp app.

###### spec {#spec-2}

[`Spec`](#spec-1)

The specification ([Spec](#spec-1)) of the app.

Build entries with the dedicated constructors ([page](#page-2), [route](#route-1), [query](#query-1), [action](#action-1), [api](#api-1), [apiNamespace](#apinamespace-1), [job](#job-1), [crud](#crud-1)).

###### title

`string`

App title.

Used as the browser tab title.

###### wasp {#wasp-2}

[`Wasp`](#wasp-1)

Wasp metadata.

This will get checked against the Wasp CLI.

###### webSocket? {#websocket-2}

[`WebSocket`](#websocket-1)

Configuration for the app's WebSocket support.

#### Returns

[`App`](#app)

#### Example

```ts
import { app, page, route } from '@wasp/spec'
import MainPage from './src/MainPage' with { type: 'ref' }

export default app({
  name: 'todoApp',
  wasp: { version: "^0.24.0" },
  title: "ToDo App",
  head: ["<link rel='icon' href='/favicon.ico' />"],
  spec: [
    route('MainRoute', '/', page(MainPage)),
  ],
})
```

## Constructors

### action() {#action-1}

> **action**(`fn`, `config?`): [`Action`](#action)

Creates an [Action](#action) definition.

Actions are server-side write operations. Like queries, they can be called from the client and the server. Listing entities in `config.entities` lets Wasp invalidate related query caches when this action runs.

See [Actions](https://wasp.sh/docs/features/data/operations/actions).

#### Parameters

##### fn {#fn-5}

[`Reference`](#reference)<[`AnyFunction`](#anyfunction)>

Reference to the Action's NodeJS implementation.

See [the docs](https://wasp.sh/docs/features/data/operations/actions#implementing-actions) for details on the implementation and its context.

Use [reference imports](https://wasp.sh/docs/features/spec#reference-imports) when passing values from your app into the Wasp spec. If a direct import is not practical, use [ref](#ref) as a fallback.

##### config?

###### auth? {#auth-6}

`boolean`

Whether this Action requires auth. If your app has auth enabled, this defaults to `true`.

Only available if your app has auth enabled.

###### entities? {#entities-5}

`string`\[]

A list of entities you wish to use inside your Action.

See [Using Entities in Actions](https://wasp.sh/docs/features/data/operations/actions#using-entities-in-actions).

#### Returns

[`Action`](#action)

#### Example

```ts
import { app, action } from "@wasp/spec"
import { createTask } from "./src/actions" with { type: "ref" }
export default app({
  // ...
  spec: [
    action(createTask, { entities: ["Task"] }),
  ],
})
```

---

### api() {#api-1}

> **api**(`method`, `path`, `fn`, `config?`): [`Api`](#api)

Creates an [Api](#api) endpoint definition.

APIs are custom HTTP endpoints handled by a plain Express function. Use them for webhooks, file uploads, or any HTTP interaction that doesn't fit the [query](#query-1)/[action](#action-1) model.

See [Custom HTTP API Endpoints](https://wasp.sh/docs/features/apis).

#### Parameters

##### method {#method-1}

[`HttpMethod`](#httpmethod)

HTTP method to listen on (or `"ALL"` for any).

##### path {#path-3}

`string`

Express path the endpoint is mounted at.

##### fn {#fn-6}

[`Reference`](#reference)<[`AnyFunction`](#anyfunction)>

The API's NodeJS implementation.

Use [reference imports](https://wasp.sh/docs/features/spec#reference-imports) when passing values from your app into the Wasp spec. If a direct import is not practical, use [ref](#ref) as a fallback.

##### config?

Optional settings: `middlewareConfigFn`, `entities`, `auth`.

###### auth? {#auth-7}

`boolean`

If `true`, the handler requires the request to come from an authenticated user and receives `context.user`. Defaults to `true` when the app has auth enabled, and `false` otherwise. Set to `false` to skip parsing the JWT from the Authorization header.

###### entities? {#entities-6}

`string`\[]

Entities the handler operates on. Wasp injects a Prisma delegate for each one into the handler's `context.entities`.

See [Using Entities in APIs](https://wasp.sh/docs/features/apis#using-entities-in-apis).

###### middlewareConfigFn? {#middlewareconfigfn-3}

[`Reference`](#reference)<[`AnyFunction`](#anyfunction)>

Reference to an Express middleware config function for this endpoint only.

See [Configuring API middleware](https://wasp.sh/docs/advanced/server-customization/middleware#2-customize-api-specific-middleware).

#### Returns

[`Api`](#api)

#### Example

```ts
import { api } from '@wasp/spec'
import { barBaz } from './src/apis' with { type: 'ref' }

api('GET', '/bar/baz', barBaz, { entities: ['Task'], auth: false })
```

---

### apiNamespace() {#apinamespace-1}

> **apiNamespace**(`path`, `config`): [`ApiNamespace`](#apinamespace)

Creates an [ApiNamespace](#apinamespace) definition.

Applies a shared middleware function to every [api](#api-1) mounted under a given path prefix. Useful for tweaking middleware (e.g. raw body parsing, CORS) for a group of related endpoints.

See the [per-path middleware section](https://wasp.sh/docs/advanced/server-customization/middleware#3-customize-per-path-middleware).

#### Parameters

##### path {#path-4}

`string`

Path prefix the namespace applies to.

##### config

Required `middlewareConfigFn`.

Use [reference imports](https://wasp.sh/docs/features/spec#reference-imports) when passing values from your app into the Wasp spec. If a direct import is not practical, use [ref](#ref) as a fallback.

###### middlewareConfigFn {#middlewareconfigfn-4}

[`Reference`](#reference)<[`AnyFunction`](#anyfunction)>

Reference to an Express middleware config function for this namespace.

#### Returns

[`ApiNamespace`](#apinamespace)

#### Example

```ts
import { apiNamespace } from '@wasp/spec'
import { barMiddleware } from './src/apis' with { type: 'ref' }

apiNamespace('/bar', { middlewareConfigFn: barMiddleware })
```

---

### crud() {#crud-1}

> **crud**(`name`, `entity`, `operations`): [`Crud`](#crud)

Creates a [Crud](#crud) definition.

Auto-generates queries and actions for a Prisma entity. Each operation in `operations` can be enabled with defaults (an empty object), made public via `isPublic`, or replaced by a custom implementation via `overrideFn`.

See [Automatic CRUD](https://wasp.sh/docs/features/data/crud).

#### Parameters

##### name {#name-5}

`string`

Unique name for the generated CRUD.

##### entity {#entity-1}

`string`

Name of the Prisma entity to generate operations for.

##### operations {#operations-1}

[`CrudOperations`](#crudoperations-1)

Which operations to generate and how to configure each.

Use [reference imports](https://wasp.sh/docs/features/spec#reference-imports) when passing values from your app into the Wasp spec. If a direct import is not practical, use [ref](#ref) as a fallback.

#### Returns

[`Crud`](#crud)

#### Example

```ts
import { crud } from '@wasp/spec'
import { createTaskOverride } from './src/actions' with { type: 'ref' }

crud('tasks', 'Task', {
  getAll: { isPublic: true },
  get: {},
  create: { overrideFn: createTaskOverride },
  update: {},
})
```

---

### job() {#job-1}

> **job**(`fn`, `config`): [`Job`](#job)

Creates a [Job](#job) definition.

Jobs are background tasks that persist across server restarts, can be retried on failure, delayed, and scheduled with cron. Pass the worker function as the first argument and configure the executor and schedule in `config`.

See [Recurring Jobs](https://wasp.sh/docs/features/jobs).

#### Parameters

##### fn {#fn-7}

[`Reference`](#reference)<[`AnyFunction`](#anyfunction)>

The async function that performs the job's work. It receives the submitted args and a context containing the declared entities.

Use [reference imports](https://wasp.sh/docs/features/spec#reference-imports) when passing values from your app into the Wasp spec. If a direct import is not practical, use [ref](#ref) as a fallback.

##### config

Required `executor` and optional `schedule`, `entities`, and `performExecutorOptions`.

###### entities? {#entities-7}

`string`\[]

Entities the worker operates on. Wasp injects a Prisma delegate for each one into the worker's `context.entities`.

This works like entity access in queries and actions. See [Using Entities in Queries](https://wasp.sh/docs/features/data/operations/queries#using-entities-in-queries).

###### executor {#executor-1}

`"PgBoss"`

Executor backing this job.

Currently only `"PgBoss"` is available.

###### performExecutorOptions? {#performexecutoroptions-1}

[`ExecutorOptions`](#executoroptions)

Executor-specific default options used when submitting the job.

These options are passed through to the executor and can be overridden when submitting the job or by [Schedule.executorOptions](#executoroptions-1) for scheduled runs. For example, with PgBoss this can set retry limits, expiration, or priority.

**Default**

```ts
{}
```

###### schedule? {#schedule-2}

[`Schedule`](#schedule-1)

Cron schedule that runs the job automatically.

#### Returns

[`Job`](#job)

#### Example

```ts
import { job } from '@wasp/spec'
import { foo } from './src/jobs/bar' with { type: 'ref' }

job(foo, {
  executor: 'PgBoss',
  entities: ['Task'],
  schedule: { cron: '0 * * * *' },
})
```

---

### page() {#page-2}

> **page**(`component`, `config?`): [`Page`](#page)

Creates a [Page](#page) definition.

A page is a React component rendered by a [route](#route-1).

See [Pages & Routes](https://wasp.sh/docs/features/pages-and-routes) and the [Auth overview](https://wasp.sh/docs/features/auth/overview#protecting-a-page-with-authrequired) for protecting pages with `authRequired`.

#### Parameters

##### component {#component-1}

[`Reference`](#reference)<[`AnyFunction`](#anyfunction)>

The React component to render.

Use [reference imports](https://wasp.sh/docs/features/spec#reference-imports) when passing values from your app into the Wasp spec. If a direct import is not practical, use [ref](#ref) as a fallback.

##### config?

Optional page settings such as `authRequired`.

###### authRequired? {#authrequired-1}

`boolean`

If `true`, only authenticated users can access this page. Unauthenticated visitors are redirected to [Auth.onAuthFailedRedirectTo](#onauthfailedredirectto).

Cannot be combined with [Route.prerender](#prerender).

**Default**

```ts
false
```

#### Returns

[`Page`](#page)

#### Example

```ts
import { page } from '@wasp/spec'
import MainPage from './src/MainPage' with { type: 'ref' }

page(MainPage, { authRequired: true })
```

---

### query() {#query-1}

> **query**(`fn`, `config?`): [`Query`](#query)

Creates a [Query](#query) definition.

Queries are server-side read-only operations. They can be invoked from the client (with caching via `useQuery`) and the server. List the entities the query reads from so Wasp can inject the matching Prisma delegates into `context.entities` and invalidate the client cache when related actions modify them.

See [Queries](https://wasp.sh/docs/features/data/operations/queries).

#### Parameters

##### fn {#fn-8}

[`Reference`](#reference)<[`AnyFunction`](#anyfunction)>

Reference to the Query's NodeJS implementation.

See [the docs](https://wasp.sh/docs/features/data/operations/queries#implementing-queries) for details on the implementation and its context.

Use [reference imports](https://wasp.sh/docs/features/spec#reference-imports) when passing values from your app into the Wasp spec. If a direct import is not practical, use [ref](#ref) as a fallback.

##### config?

###### auth? {#auth-8}

`boolean`

Whether this Query requires auth. If your app has auth enabled, this defaults to `true`.

Only available if your app has auth enabled.

###### entities? {#entities-8}

`string`\[]

A list of entities you wish to use inside your Query.

See [Using Entities in Queries](https://wasp.sh/docs/features/data/operations/queries#using-entities-in-queries).

#### Returns

[`Query`](#query)

#### Example

```ts
import { app, query } from "@wasp/spec"
import { getTasks } from './src/queries' with { type: 'ref' }

export default app({
  // ...
  spec: [
    query(getTasks, { entities: ["Foo"] }),
  ],
})
```

---

### route() {#route-1}

> **route**(`name`, `path`, `page`, `config?`): [`Route`](#route)

Creates a [Route](#route) definition.

Maps a URL path to a [Page](#page). Paths support React Router patterns such as dynamic segments (`/tasks/:id`), optional segments (`/photo/:id/edit?`), and splats (`/files/*`).

Use `config.prerender` to render the route to static HTML at build time: `true` prerenders the route's own static path, or pass an array of concrete paths to prerender specific instances of a dynamic route (see [Prerendering](https://wasp.sh/docs/advanced/prerendering)). Use `config.lazy` to opt out of lazy-loading the page's bundle.

#### Parameters

##### name {#name-6}

`string`

Unique name for the route.

##### path {#path-5}

`string`

URL path the route matches.

##### page {#page-3}

[`Page`](#page)

The result of `page()`.

##### config?

Optional route settings (`lazy`, `prerender`).

###### lazy? {#lazy-1}

`boolean`

Lazy-load the page's component.

Set to `false` to include the page in the initial client bundle, which avoids the brief loading delay on first navigation at the cost of a larger initial download.

**Default**

```ts
true
```

###### prerender? {#prerender-1}

`boolean` | readonly `string`\[]

Render this route to static HTML at build time. Useful for SEO and AI crawlers. The page then hydrates on the client for full interactivity.

Accepts either:

- `true` — prerender this route's own path. The path must be fully static (no `:paramName`, `*`, or `?` segments).
- an array of concrete paths to prerender. Use this to prerender specific instances of a dynamic route (e.g. `["/blog/intro", "/blog/changelog"]` for a `"/blog/:slug"` route). Every listed path must be fully static and must match this route's path pattern.

In either case the route's page cannot have [Page.authRequired](#authrequired) set, since prerendered content can't depend on the logged-in user. See [Prerendering](https://wasp.sh/docs/advanced/prerendering).

**Default**

```ts
false
```

**Example**

```ts
import { app, page, route } from "@wasp/spec"
import { LandingPage } from "./src/LandingPage" with { type: "ref" }
import { BlogPostPage } from "./src/BlogPostPage" with { type: "ref" }

export default app({
  // ...
  spec: [
    route("LandingRoute", "/", page(LandingPage), {
      prerender: true,
    }),
    route("BlogPostRoute", "/blog/:slug", page(BlogPostPage), {
      prerender: ["/blog/intro", "/blog/changelog"],
    }),
  ],
})
```

#### Returns

[`Route`](#route)

#### Example

```ts
import { page, route } from '@wasp/spec'
import MainPage from './src/MainPage' with { type: 'ref' }

route('MainRoute', '/', page(MainPage))
```

## Specifications

### Action {#action}

A server-side write operation, callable from the client and the server.

Create one with the [action](#action-1) constructor.

See [Actions](https://wasp.sh/docs/features/data/operations/actions).

#### Extends

- [`BaseSpecElement`](#basespecelement)<`"action"`>

#### Properties

##### auth? {#auth}

> `optional` **auth?**: `boolean`

Whether this Action requires auth. If your app has auth enabled, this defaults to `true`.

Only available if your app has auth enabled.

##### entities? {#entities}

> `optional` **entities?**: `string`\[]

A list of entities you wish to use inside your Action.

See [Using Entities in Actions](https://wasp.sh/docs/features/data/operations/actions#using-entities-in-actions).

##### fn {#fn}

> **fn**: [`Reference`](#reference)<[`AnyFunction`](#anyfunction)>

Reference to the Action's NodeJS implementation.

See [the docs](https://wasp.sh/docs/features/data/operations/actions#implementing-actions) for details on the implementation and its context.

##### kind

> **kind**: `"action"`

The internal Wasp type of a [SpecElement](#specelement). Used by the compiler. You should not set this field directly, instead use the dedicated constructors.

###### Inherited from

[`BaseSpecElement`](#basespecelement).[`kind`](#kind-4)

---

### Api {#api}

A custom HTTP endpoint mounted on the Wasp server.

Create one with the [api](#api-1) constructor.

Unlike operations, APIs are plain Express handlers that receive `req`, `res`, and a Wasp `context`. They are useful for webhooks and any non-standard HTTP interaction.

See [Custom HTTP API Endpoints](https://wasp.sh/docs/features/apis).

#### Extends

- [`BaseSpecElement`](#basespecelement)<`"api"`>

#### Properties

##### auth? {#auth-1}

> `optional` **auth?**: `boolean`

If `true`, the handler requires the request to come from an authenticated user and receives `context.user`. Defaults to `true` when the app has auth enabled, and `false` otherwise. Set to `false` to skip parsing the JWT from the Authorization header.

##### entities? {#entities-1}

> `optional` **entities?**: `string`\[]

Entities the handler operates on. Wasp injects a Prisma delegate for each one into the handler's `context.entities`.

See [Using Entities in APIs](https://wasp.sh/docs/features/apis#using-entities-in-apis).

##### fn {#fn-1}

> **fn**: [`Reference`](#reference)<[`AnyFunction`](#anyfunction)>

Reference to the API's NodeJS implementation.

##### kind

> **kind**: `"api"`

The internal Wasp type of a [SpecElement](#specelement). Used by the compiler. You should not set this field directly, instead use the dedicated constructors.

###### Inherited from

[`BaseSpecElement`](#basespecelement).[`kind`](#kind-4)

##### method {#method}

> **method**: [`HttpMethod`](#httpmethod)

HTTP method this endpoint responds to.

##### middlewareConfigFn? {#middlewareconfigfn}

> `optional` **middlewareConfigFn?**: [`Reference`](#reference)<[`AnyFunction`](#anyfunction)>

Reference to an Express middleware config function for this endpoint only.

See [Configuring API middleware](https://wasp.sh/docs/advanced/server-customization/middleware#2-customize-api-specific-middleware).

##### path {#path}

> **path**: `string`

Express path of the endpoint (e.g. `"/webhooks/stripe"`).

---

### ApiNamespace {#apinamespace}

Shared middleware applied to every [Api](#api) mounted under a path prefix.

Create one with the [apiNamespace](#apinamespace-1) constructor.

#### Extends

- [`BaseSpecElement`](#basespecelement)<`"apiNamespace"`>

#### Properties

##### kind

> **kind**: `"apiNamespace"`

The internal Wasp type of a [SpecElement](#specelement). Used by the compiler. You should not set this field directly, instead use the dedicated constructors.

###### Inherited from

[`BaseSpecElement`](#basespecelement).[`kind`](#kind-4)

##### middlewareConfigFn {#middlewareconfigfn-1}

> **middlewareConfigFn**: [`Reference`](#reference)<[`AnyFunction`](#anyfunction)>

Reference to an Express middleware config function for this namespace.

##### path {#path-1}

> **path**: `string`

Path prefix the namespace applies to (e.g. `"/webhooks"`).

---

### Crud {#crud}

Auto-generated CRUD operations for an entity.

Create one with the [crud](#crud-1) constructor.

See [Automatic CRUD](https://wasp.sh/docs/features/data/crud).

#### Extends

- [`BaseSpecElement`](#basespecelement)<`"crud"`>

#### Properties

##### entity {#entity}

> **entity**: `string`

Entity to generate operations for.

##### kind {#kind-5}

> **kind**: `"crud"`

The internal Wasp type of a [SpecElement](#specelement). Used by the compiler. You should not set this field directly, instead use the dedicated constructors.

###### Inherited from

[`BaseSpecElement`](#basespecelement).[`kind`](#kind-4)

##### name {#name-1}

> **name**: `string`

Unique name for this CRUD.

##### operations {#operations}

> **operations**: [`CrudOperations`](#crudoperations-1)

Which operations to generate and how to configure each one.

---

### Job {#job}

A background job. Can be submitted ad-hoc or run on a recurring schedule.

Create one with the [job](#job-1) constructor.

See [Recurring Jobs](https://wasp.sh/docs/features/jobs).

#### Extends

- [`BaseSpecElement`](#basespecelement)<`"job"`>

#### Properties

##### entities? {#entities-2}

> `optional` **entities?**: `string`\[]

Entities the worker operates on. Wasp injects a Prisma delegate for each one into the worker's `context.entities`.

This works like entity access in queries and actions. See [Using Entities in Queries](https://wasp.sh/docs/features/data/operations/queries#using-entities-in-queries).

##### executor {#executor}

> **executor**: `"PgBoss"`

Executor backing this job.

Currently only `"PgBoss"` is available.

##### fn {#fn-2}

> **fn**: [`Reference`](#reference)<[`AnyFunction`](#anyfunction)>

Reference to the job's NodeJS implementation. It receives the submitted args and a context containing the declared entities.

See [Jobs documentation](https://wasp.sh/docs/features/jobs#worker-api) for more details.

##### kind {#kind-6}

> **kind**: `"job"`

The internal Wasp type of a [SpecElement](#specelement). Used by the compiler. You should not set this field directly, instead use the dedicated constructors.

###### Inherited from

[`BaseSpecElement`](#basespecelement).[`kind`](#kind-4)

##### performExecutorOptions? {#performexecutoroptions}

> `optional` **performExecutorOptions?**: [`ExecutorOptions`](#executoroptions)

Executor-specific default options used when submitting the job.

These options are passed through to the executor and can be overridden when submitting the job or by [Schedule.executorOptions](#executoroptions-1) for scheduled runs. For example, with PgBoss this can set retry limits, expiration, or priority.

###### Default

```ts
{}
```

##### schedule? {#schedule}

> `optional` **schedule?**: [`Schedule`](#schedule-1)

Cron schedule that runs the job automatically.

---

### Page {#page}

A page in the app, normally a React component rendered for a [Route](#route).

Create one with the [page](#page-2) constructor.

#### Extends

- [`BaseSpecElement`](#basespecelement)<`"page"`>

#### Properties

##### authRequired? {#authrequired}

> `optional` **authRequired?**: `boolean`

If `true`, only authenticated users can access this page. Unauthenticated visitors are redirected to [Auth.onAuthFailedRedirectTo](#onauthfailedredirectto).

Cannot be combined with [Route.prerender](#prerender).

###### Default

```ts
false
```

##### component {#component}

> **component**: [`Reference`](#reference)<[`AnyFunction`](#anyfunction)>

React component rendered for this page.

##### kind {#kind-7}

> **kind**: `"page"`

The internal Wasp type of a [SpecElement](#specelement). Used by the compiler. You should not set this field directly, instead use the dedicated constructors.

###### Inherited from

[`BaseSpecElement`](#basespecelement).[`kind`](#kind-4)

---

### Query {#query}

A server-side read-only operation, callable from the client and the server.

Create one with the [query](#query-1) constructor.

See [Queries](https://wasp.sh/docs/features/data/operations/queries).

#### Extends

- [`BaseSpecElement`](#basespecelement)<`"query"`>

#### Properties

##### auth? {#auth-4}

> `optional` **auth?**: `boolean`

Whether this Query requires auth. If your app has auth enabled, this defaults to `true`.

Only available if your app has auth enabled.

##### entities? {#entities-3}

> `optional` **entities?**: `string`\[]

A list of entities you wish to use inside your Query.

See [Using Entities in Queries](https://wasp.sh/docs/features/data/operations/queries#using-entities-in-queries).

##### fn {#fn-3}

> **fn**: [`Reference`](#reference)<[`AnyFunction`](#anyfunction)>

Reference to the Query's NodeJS implementation.

See [the docs](https://wasp.sh/docs/features/data/operations/queries#implementing-queries) for details on the implementation and its context.

##### kind {#kind-8}

> **kind**: `"query"`

The internal Wasp type of a [SpecElement](#specelement). Used by the compiler. You should not set this field directly, instead use the dedicated constructors.

###### Inherited from

[`BaseSpecElement`](#basespecelement).[`kind`](#kind-4)

---

### Route {#route}

A URL path mapped to a [Page](#page).

Create one with the [route](#route-1) constructor.

See [Pages & Routes](https://wasp.sh/docs/features/pages-and-routes) for path patterns (dynamic segments, optional segments, splats).

#### Extends

- [`BaseSpecElement`](#basespecelement)<`"route"`>

#### Properties

##### kind {#kind-9}

> **kind**: `"route"`

The internal Wasp type of a [SpecElement](#specelement). Used by the compiler. You should not set this field directly, instead use the dedicated constructors.

###### Inherited from

[`BaseSpecElement`](#basespecelement).[`kind`](#kind-4)

##### lazy? {#lazy}

> `optional` **lazy?**: `boolean`

Lazy-load the page's component.

Set to `false` to include the page in the initial client bundle, which avoids the brief loading delay on first navigation at the cost of a larger initial download.

###### Default

```ts
true
```

##### name {#name-3}

> **name**: `string`

Unique route name.

##### page {#page-1}

> **page**: [`Page`](#page)

Page rendered when this route matches.

##### path {#path-2}

> **path**: `string`

URL path. Supports React Router patterns like `/tasks/:id`, `/photo/:id/edit?`, and `/files/*`.

##### prerender? {#prerender}

> `optional` **prerender?**: `boolean` | readonly `string`\[]

Render this route to static HTML at build time. Useful for SEO and AI crawlers. The page then hydrates on the client for full interactivity.

Accepts either:

- `true` — prerender this route's own path. The path must be fully static (no `:paramName`, `*`, or `?` segments).
- an array of concrete paths to prerender. Use this to prerender specific instances of a dynamic route (e.g. `["/blog/intro", "/blog/changelog"]` for a `"/blog/:slug"` route). Every listed path must be fully static and must match this route's path pattern.

In either case the route's page cannot have [Page.authRequired](#authrequired) set, since prerendered content can't depend on the logged-in user. See [Prerendering](https://wasp.sh/docs/advanced/prerendering).

###### Default

```ts
false
```

###### Example

```ts
import { app, page, route } from "@wasp/spec"
import { LandingPage } from "./src/LandingPage" with { type: "ref" }
import { BlogPostPage } from "./src/BlogPostPage" with { type: "ref" }

export default app({
  // ...
  spec: [
    route("LandingRoute", "/", page(LandingPage), {
      prerender: true,
    }),
    route("BlogPostRoute", "/blog/:slug", page(BlogPostPage), {
      prerender: ["/blog/intro", "/blog/changelog"],
    }),
  ],
})
```

---

### Spec {#spec-1}

> **Spec** = [`SpecElement`](#specelement) | [`Spec`](#spec-1)\[]

A single [SpecElement](#specelement), or an (optionally nested) array of them, that makes up the [App.spec](#spec).

This lets you define related elements separately (for example, one array per feature) and compose them together. The nested structure is treated as a single flat list of elements.

#### Example

Nested arrays let you compose a spec from separate sub-specs:

```ts
import { app } from "@wasp/spec"

const authSpec: Spec = [signupRoute, loginRoute];
const tasksSpec: Spec = [tasksRoute, getTasks, createTask];

export default app({
  // ...
  spec: [authSpec, tasksSpec],
});
```

---

### SpecElement

> **SpecElement** = [`Page`](#page) | [`Route`](#route) | [`Query`](#query) | [`Action`](#action) | [`Api`](#api) | [`ApiNamespace`](#apinamespace) | [`Job`](#job) | [`Crud`](#crud)

Union of every kind of specification element that can appear in [App.spec](#spec).

Each variant is produced by its matching constructor.

## References

### DefaultRefObjectDescriptor

Default import reference, equivalent to `import SomeValue from "./src/someModule" with { type: "ref" }`.

#### Properties

##### from

> **from**: `string`

Module path, relative to the `*.wasp.ts` file using it.

##### importDefault

> **importDefault**: `string`

Local name for the default import.

---

### NamedRefObjectDescriptor

Named import reference, equivalent to `import { SomeValue } from "./src/someModule" with { type: "ref" }`.

#### Properties

##### alias?

> `optional` **alias?**: `string`

Optional local alias.

Alias takes precedence over the `import` field when Wasp Spec dervies some [WaspSpec.SpecElement](#specelement) name.

##### from

> **from**: `string`

Module path, relative to the `*.wasp.ts` file using it.

##### import

> **import**: `string`

Exported name to import.

---

### Reference

> **Reference**<`AppValue`> = [`RefObject`](#refobject) | `AppValue`

Represents a value from your app passed into the Wasp spec.

Use [reference imports](https://wasp.sh/docs/features/spec#reference-imports) when passing values from your app into the Wasp spec. If a direct import is not practical, use [ref](#ref) as a fallback.

#### Type Parameters

##### AppValue

`AppValue`

The expected TypeScript type for the value from your app. The field using `Reference<AppValue>` decides this type. For example, [Page.component](#component) expects a React component, [Query.fn](#fn-3) expects a server function, and env schema fields expect a [ZodSchema](#zodschema).

#### Example

```ts
import { app, query } from "@wasp/spec"
import { getTasks } from "./src/queries" with { type: "ref" }

export default app({
  // ...
  spec: [
    query(getTasks, { entities: ["Task"] }),
  ],
})
```

---

### RefObject

> **RefObject** = [`Branded`](#branded)<[`RefObjectDescriptor`](#refobjectdescriptor) & `object`, `"RefObject"`>

A reference to code in your app's `src` directory.

---

### RefObjectDescriptor

> **RefObjectDescriptor** = [`NamedRefObjectDescriptor`](#namedrefobjectdescriptor) | [`DefaultRefObjectDescriptor`](#defaultrefobjectdescriptor)

Input accepted by [ref](#ref).

---

### ZodSchema

> **ZodSchema** = `object`

Structural type for a runtime Zod schema accepted by Wasp env validation fields.

Use [reference imports](https://wasp.sh/docs/features/spec#reference-imports) when passing values from your app into the Wasp spec. If a direct import is not practical, use [ref](#ref) as a fallback.

See [Server.envValidationSchema](#envvalidationschema-1) and [Client.envValidationSchema](#envvalidationschema) for examples.

To avoid depending on the `zod` package, Wasp structurally recognizes Zod 4 schemas via their documented library-author marker. See <https://zod.dev/library-authors>.

#### Properties

##### \_zod

> `readonly` **\_zod**: `object`

###### def

> `readonly` **def**: `object`

---

### ref()

> **ref**(`_descriptor`): [`RefObject`](#refobject)

Creates a fallback reference object for a value from your app.

Use [reference imports](https://wasp.sh/docs/features/spec#reference-imports) when passing values from your app into the Wasp spec. If a direct import is not practical, use [ref](#ref) as a fallback.

Reference imports are preferred because editors can follow and rename real imports.

The import path must be relative to the `*.wasp.ts` file where it is used and resolve inside the app's `src/` directory. Absolute paths are not supported.

#### Parameters

##### \_descriptor

[`RefObjectDescriptor`](#refobjectdescriptor)

#### Returns

[`RefObject`](#refobject)

#### Example

```ts
import { page, ref } from "@wasp/spec"

const MainPage = ref({
  importDefault: "MainPage",
  from: "./src/MainPage",
})

export const mainPage = page(MainPage)
```

## Errors

### WaspSpecUserError

Error caused by invalid user input (unknown entities, bad imports, etc.).

Wasp's spec pipeline throws `WaspSpecUserError` when it finds a problem with the user's configuration. Its message is reported to the user as a clean, actionable error instead of an internal stack trace.

Throw it from your own spec helper libraries when you want to report a configuration problem the same way Wasp does:

#### Example

```ts
import { WaspSpecUserError } from "@wasp/spec"

export function requireEnv(name: string): string {
  const value = process.env[name]
  if (value === undefined) {
    throw new WaspSpecUserError(`Missing required environment variable '${name}'.`)
  }
  return value
}
```

These are the errors users hit most often during development, so keep their messages short, clear, and actionable.

#### Extends

- `Error`

#### Constructors

##### Constructor

> **new WaspSpecUserError**(`message?`): [`WaspSpecUserError`](#waspspecusererror)

###### Parameters

###### message?

`string`

###### Returns

[`WaspSpecUserError`](#waspspecusererror)

###### Inherited from

`Error.constructor`

##### Constructor

> **new WaspSpecUserError**(`message?`, `options?`): [`WaspSpecUserError`](#waspspecusererror)

###### Parameters

###### message?

`string`

###### options?

`ErrorOptions`

###### Returns

[`WaspSpecUserError`](#waspspecusererror)

###### Inherited from

`Error.constructor`

## Fields

### Auth {#auth-3}

Authentication configuration and lifecycle hooks.

See the [Auth overview](https://wasp.sh/docs/features/auth/overview) for the supported auth methods and how the `User` entity is connected to auth. If hooks are async, Wasp awaits them. All hooks receive `prisma` and `req` in their input. Hook return values are ignored except for [AuthHooks.onBeforeOAuthRedirect](#onbeforeoauthredirect), which can change the redirect URL.

In TypeScript, you can type each hook implementation with its matching type from `wasp/server/auth` (e.g. `OnBeforeSignupHook`). See [Auth Hooks](https://wasp.sh/docs/features/auth/hooks) for the full hook inputs and examples.

#### Example

```ts
import { app } from "@wasp/spec"

export default app({
  // ...
  auth: {
    userEntity: "User",
    methods: {
      usernameAndPassword: {}, // use this or email, not both
      google: {},
      gitHub: {},
    },
    onAuthFailedRedirectTo: "/login",
  },
})
```

#### Extends

- [`AuthHooks`](#authhooks)

#### Fields

##### methods

> **methods**: [`AuthMethods`](#authmethods-1)

Enabled authentication methods.

##### onAfterEmailVerified?

> `optional` **onAfterEmailVerified?**: [`Reference`](#reference)<[`AnyFunction`](#anyfunction)>

Called once, after the user verifies their email. Receives `email` and `user`.

###### Inherited from

[`AuthHooks`](#authhooks).[`onAfterEmailVerified`](#onafteremailverified-1)

##### onAuthFailedRedirectTo

> **onAuthFailedRedirectTo**: `string`

Route that Wasp redirects unauthenticated users to when they try to access a page that has `authRequired: true`.

See [Adding Auth to the Project](https://wasp.sh/docs/tutorial/auth#adding-auth-to-the-project) for an example.

##### onAuthSucceededRedirectTo?

> `optional` **onAuthSucceededRedirectTo?**: `string`

Route that Wasp redirects users to after a successful login or signup.

Only takes effect when using Wasp's built-in Auth UI.

See [Auth UI](https://wasp.sh/docs/features/auth/ui).

###### Default

```ts
"/"
```

##### userEntity

> **userEntity**: `string`

Name of the Prisma model that represents the application user connected to your business logic.

The user entity needs to have an ID field that uniquely identifies each user. It can be of any name and type, but it needs to be marked with `@id`:

```prisma title="schema.prisma"
model User {
  id Int @id @default(autoincrement())
}
```

You can add any other fields you want to the user entity. Make sure to also define them in the `userSignupFields` field if they need to be set during the sign-up process.

See [Accessing User Data](https://wasp.sh/docs/features/auth/entities) for how the user entity connects to the rest of the auth system.

#### Hooks

##### onAfterLogin?

> `optional` **onAfterLogin?**: [`Reference`](#reference)<[`AnyFunction`](#anyfunction)>

Called after a successful login. Receives `providerId`, `user`, and, for social auth, `oauth` fields including tokens and the unique OAuth request ID.

###### Inherited from

[`AuthHooks`](#authhooks).[`onAfterLogin`](#onafterlogin-1)

##### onAfterSignup?

> `optional` **onAfterSignup?**: [`Reference`](#reference)<[`AnyFunction`](#anyfunction)>

Called after the user is created. Receives `providerId`, the created `user`, and, for social auth, `oauth` fields including tokens and the unique OAuth request ID.

###### Inherited from

[`AuthHooks`](#authhooks).[`onAfterSignup`](#onaftersignup-1)

##### onBeforeLogin?

> `optional` **onBeforeLogin?**: [`Reference`](#reference)<[`AnyFunction`](#anyfunction)>

Called before the user is logged in. Receives `providerId` and `user`. Throw from this hook to reject a login based on custom criteria.

###### Inherited from

[`AuthHooks`](#authhooks).[`onBeforeLogin`](#onbeforelogin-1)

##### onBeforeOAuthRedirect?

> `optional` **onBeforeOAuthRedirect?**: [`Reference`](#reference)<[`AnyFunction`](#anyfunction)>

Called before redirecting the user to the OAuth provider. Receives the generated `url` and `oauth.uniqueRequestId`. Return `{ url }` to override the redirect URL.

###### Inherited from

[`AuthHooks`](#authhooks).[`onBeforeOAuthRedirect`](#onbeforeoauthredirect-1)

##### onBeforeSignup?

> `optional` **onBeforeSignup?**: [`Reference`](#reference)<[`AnyFunction`](#anyfunction)>

Called before the user is created. Receives `providerId` plus the common hook input. Throw from this hook to reject a signup based on custom criteria.

###### Inherited from

[`AuthHooks`](#authhooks).[`onBeforeSignup`](#onbeforesignup-1)

---

### AuthHooks

#### Extended by

- [`Auth`](#auth-3)

#### Fields

##### onAfterEmailVerified?

> `optional` **onAfterEmailVerified?**: [`Reference`](#reference)<[`AnyFunction`](#anyfunction)>

Called once, after the user verifies their email. Receives `email` and `user`.

#### Hooks

##### onAfterLogin?

> `optional` **onAfterLogin?**: [`Reference`](#reference)<[`AnyFunction`](#anyfunction)>

Called after a successful login. Receives `providerId`, `user`, and, for social auth, `oauth` fields including tokens and the unique OAuth request ID.

##### onAfterSignup?

> `optional` **onAfterSignup?**: [`Reference`](#reference)<[`AnyFunction`](#anyfunction)>

Called after the user is created. Receives `providerId`, the created `user`, and, for social auth, `oauth` fields including tokens and the unique OAuth request ID.

##### onBeforeLogin?

> `optional` **onBeforeLogin?**: [`Reference`](#reference)<[`AnyFunction`](#anyfunction)>

Called before the user is logged in. Receives `providerId` and `user`. Throw from this hook to reject a login based on custom criteria.

##### onBeforeOAuthRedirect?

> `optional` **onBeforeOAuthRedirect?**: [`Reference`](#reference)<[`AnyFunction`](#anyfunction)>

Called before redirecting the user to the OAuth provider. Receives the generated `url` and `oauth.uniqueRequestId`. Return `{ url }` to override the redirect URL.

##### onBeforeSignup?

> `optional` **onBeforeSignup?**: [`Reference`](#reference)<[`AnyFunction`](#anyfunction)>

Called before the user is created. Receives `providerId` plus the common hook input. Throw from this hook to reject a signup based on custom criteria.

---

### BaseAuthMethodConfig

#### Extended by

- [`UsernameAndPasswordConfig`](#usernameandpasswordconfig)
- [`SocialAuthConfig`](#socialauthconfig)
- [`EmailAuthConfig`](#emailauthconfig)

#### Properties

##### userSignupFields?

> `optional` **userSignupFields?**: [`Reference`](#reference)<[`AnyObject`](#anyobject)>

Object that defines extra fields to save on the user during signup (e.g. `firstName`, `address`). Each field name must exist on the configured [Auth.userEntity](#userentity).

Each field function receives the data sent from the client and returns the value Wasp saves to the database. For social auth, this data includes provider-specific profile information. If the value is invalid, throw an error. The `password` field is excluded from this object and handled by Wasp's auth backend.

See [Signup Fields Customization](https://wasp.sh/docs/features/auth/overview#signup-fields-customization).

###### Example

```ts
import { defineUserSignupFields } from 'wasp/server/auth'

export const userSignupFields = defineUserSignupFields({
  address: (data) => {
    if (!data.address) {
      throw new Error('Address is required')
    }
    return data.address
  },
  phone: (data) => data.phone,
})
```

---

### BaseSpecElement

#### Extended by

- [`Page`](#page)
- [`Route`](#route)
- [`Query`](#query)
- [`Action`](#action)
- [`Api`](#api)
- [`ApiNamespace`](#apinamespace)
- [`Job`](#job)
- [`Crud`](#crud)

#### Type Parameters

##### Kind {#kind-3}

`Kind` *extends* `string`

#### Properties

##### kind {#kind-4}

> **kind**: `Kind`

The internal Wasp type of a [SpecElement](#specelement). Used by the compiler. You should not set this field directly, instead use the dedicated constructors.

---

### Client {#client-1}

Client-side application configuration.

See [Client Config](https://wasp.sh/docs/advanced/client-customization/client-config) for usage details.

#### Example

```ts
import { app } from "@wasp/spec"
import Root from "./src/Root" with { type: "ref" }
import mySetupFunction from "./src/myClientSetupCode" with { type: "ref" }

export default app({
  // ...
  client: {
    rootComponent: Root,
    setupFn: mySetupFunction,
    baseDir: "/my-app",
  },
})
```

#### Properties

##### baseDir?

> `optional` **baseDir?**: `` `/${string}` ``

If you need to serve the client from a subdirectory, you can use the `baseDir` option.

If you set `baseDir` to `/my-app` for example, that will make Wasp set the `basename` prop of the `Router` to `/my-app`. It will also set the `base` option of the Vite config to `/my-app`.

This means that if you serve your app from `https://example.com/my-app`, the router will work correctly, and all the assets will be served from `https://example.com/my-app`.

Check the [Base directory docs](https://wasp.sh/docs/advanced/client-customization/client-config#base-directory) for important details.

##### envValidationSchema?

> `optional` **envValidationSchema?**: [`Reference`](#reference)<{ `_zod`: { `def`: `object`; }; }>

Zod schema used to validate user-defined client environment variables at build time. Wasp merges it with built-in validation for Wasp-defined env vars when validating `import.meta.env`. Client env vars must be prefixed with `REACT_APP_`. Define the schema with `defineEnvValidationSchema` from `wasp/env` to make sure it is type-checked.

Use [reference imports](https://wasp.sh/docs/features/spec#reference-imports) when passing values from your app into the Wasp spec. If a direct import is not practical, use [ref](#ref) as a fallback.

For example:

```ts
import { clientEnvSchema } from './src/env' with { type: 'ref' }
```

See [Env Vars](https://wasp.sh/docs/advanced/env-vars).

##### rootComponent?

> `optional` **rootComponent?**: [`Reference`](#reference)<[`AnyFunction`](#anyfunction)>

Wrapper React component rendered at the root of the client app. Must render either `<Outlet>` from `react-router` or the `children` prop to show the current page.

Useful for layouts, providers (Redux, theme, etc.), and global UI.

###### Example

```tsx
import { Outlet } from "react-router"
import store from "./store"
import { Provider } from "react-redux"

export default function Root() {
  return (
    <Provider store={store}>
      <Layout />
    </Provider>
  )
}

function Layout() {
  return (
    <div>
      <header>
        <h1>My App</h1>
      </header>

      {/* The current page will be rendered here */}
      <Outlet />

      <footer>
        <p>My App footer</p>
      </footer>
    </div>
  )
}
```

##### setupFn?

> `optional` **setupFn?**: [`Reference`](#reference)<[`AnyFunction`](#anyfunction)>

Async function Wasp calls once while initializing the client app. Wasp awaits it before rendering the app. It receives no arguments, and its return value is ignored. Use it for one-time client-side setup, such as configuring the query client or starting client-side periodic jobs.

###### Example

```ts
export default async function mySetupFunction(): Promise<void> {
  // Run some code
}
```

---

### CrudOperationOptions

Configuration for a single CRUD operation.

#### Properties

##### isPublic?

> `optional` **isPublic?**: `boolean`

If `true`, the operation skips Wasp's auth check and can be called by anyone.

###### Default

```ts
false
```

##### overrideFn?

> `optional` **overrideFn?**: [`Reference`](#reference)<[`AnyFunction`](#anyfunction)>

Reference to a custom implementation that replaces Wasp's auto-generated one. Use this when you need custom business logic for an operation (e.g. attaching `userId` on `create`).

See the [CRUD guide](https://wasp.sh/docs/features/data/crud#adding-crud-to-the-task-entity-) for an override example.

The override receives the caller-provided `args` payload and a Wasp-provided `context` containing the current user and the entity being operated on.

**Important** Use an override to validate or filter input before saving it; the default `create` and `update` implementations pass client-sent data to Prisma.

---

### CrudOperations {#crudoperations-1}

Mapping of CRUD operations to their options.

Each key enables the matching operation; an empty object enables it with Wasp's defaults.

The default implementations map directly to Prisma calls on the entity, and use the field marked with `@id` in the Prisma schema as the entity ID. Unless an operation is marked with [CrudOperationOptions.isPublic](#ispublic), Wasp checks that an authenticated user is making the request.

CRUD operations are implemented with Wasp queries and actions. See [Operations](https://wasp.sh/docs/features/data/operations/overview).

#### Properties

##### create?

> `optional` **create?**: [`CrudOperationOptions`](#crudoperationoptions)

###### Default

`Entity.create({ data: args })`

##### delete?

> `optional` **delete?**: [`CrudOperationOptions`](#crudoperationoptions)

###### Default

`Entity.delete({ where: { id: args.id } })`

##### get?

> `optional` **get?**: [`CrudOperationOptions`](#crudoperationoptions)

###### Default

`Entity.findUnique({ where: { id: args.id } })`

##### getAll?

> `optional` **getAll?**: [`CrudOperationOptions`](#crudoperationoptions)

###### Default

`Entity.findMany()`

##### update?

> `optional` **update?**: [`CrudOperationOptions`](#crudoperationoptions)

###### Default

`Entity.update({ where: { id }, data })`, where `{ id, ...data } = args`

---

### Db {#db-1}

Database configuration.

See [Databases](https://wasp.sh/docs/features/data/databases) for seeding and Prisma client customization.

#### Example

```ts
import { app } from "@wasp/spec"
import devSeed from "./src/dbSeeds" with { type: "ref" }
import { setUpPrisma } from "./src/prisma" with { type: "ref" }

export default app({
  // ...
  db: {
    seeds: [devSeed],
    prismaSetupFn: setUpPrisma,
  },
})
```

#### Properties

##### prismaSetupFn?

> `optional` **prismaSetupFn?**: [`Reference`](#reference)<[`AnyFunction`](#anyfunction)>

Function that sets up and returns a configured Prisma Client instance. Use this to add Prisma logging or client extensions.

See Prisma's [logging](https://www.prisma.io/docs/orm/prisma-client/observability-and-logging/logging) and [client extensions](https://www.prisma.io/docs/orm/prisma-client/client-extensions) docs.

###### Example

```ts
import { PrismaClient } from "@prisma/client"

export const setUpPrisma = () => {
  const prisma = new PrismaClient({
    log: ["query", "info", "warn", "error"],
  })

  return prisma
}
```

##### seeds?

> `optional` **seeds?**: [`Reference`](#reference)<[`AnyFunction`](#anyfunction)>\[]

Async functions runnable with `wasp db seed [name]` to populate the database with initial data. Each function receives Wasp's Prisma Client; the name passed to `wasp db seed` matches the function's identifier in the import.

See [Seeding the Database](https://wasp.sh/docs/features/data/databases#seeding-the-database).

---

### Deployment {#deployment-1}

Deployment configuration for the app.

#### Properties

##### mode?

> `optional` **mode?**: `"split"`

How the client and server are deployed.

###### Default

```ts
"split"
```

---

### EmailAuthConfig

Email auth configuration.

See the [Email auth docs](https://wasp.sh/docs/features/auth/email/overview) for the full signup, verification, and password reset flows.

#### Example

```ts
import { app } from "@wasp/spec"
import { userSignupFields } from "./src/auth" with { type: "ref" }
import {
  getVerificationEmailContent,
  getPasswordResetEmailContent,
} from "./src/auth/email" with { type: "ref" }

export default app({
  // ...
  auth: {
    userEntity: "User",
    methods: {
      email: {
        userSignupFields,
        fromField: {
          name: "My App",
          email: "hello@itsme.com",
        },
        emailVerification: {
          clientRoute: "EmailVerificationRoute",
          getEmailContentFn: getVerificationEmailContent,
        },
        passwordReset: {
          clientRoute: "PasswordResetRoute",
          getEmailContentFn: getPasswordResetEmailContent,
        },
      },
    },
    onAuthFailedRedirectTo: "/login",
  },
})
```

#### Extends

- [`BaseAuthMethodConfig`](#baseauthmethodconfig)

#### Properties

##### emailVerification

> **emailVerification**: [`EmailFlowConfig`](#emailflowconfig)

An object that specifies the details of the e-mail verification process.

##### fromField

> **fromField**: [`EmailFromField`](#emailfromfield)

Sender identity used for verification and password reset emails.

##### passwordReset

> **passwordReset**: [`EmailFlowConfig`](#emailflowconfig)

An object that specifies the password reset process.

##### userSignupFields?

> `optional` **userSignupFields?**: [`Reference`](#reference)<[`AnyObject`](#anyobject)>

Object that defines extra fields to save on the user during signup (e.g. `firstName`, `address`). Each field name must exist on the configured [Auth.userEntity](#userentity).

Each field function receives the data sent from the client and returns the value Wasp saves to the database. For social auth, this data includes provider-specific profile information. If the value is invalid, throw an error. The `password` field is excluded from this object and handled by Wasp's auth backend.

See [Signup Fields Customization](https://wasp.sh/docs/features/auth/overview#signup-fields-customization).

###### Example

```ts
import { defineUserSignupFields } from 'wasp/server/auth'

export const userSignupFields = defineUserSignupFields({
  address: (data) => {
    if (!data.address) {
      throw new Error('Address is required')
    }
    return data.address
  },
  phone: (data) => data.phone,
})
```

###### Inherited from

[`BaseAuthMethodConfig`](#baseauthmethodconfig).[`userSignupFields`](#usersignupfields)

---

### EmailFlowConfig

Configuration for an email-based auth flow (verification or password reset).

#### Properties

##### clientRoute

> **clientRoute**: `string`

Name of the route that handles the link sent in the email (e.g. `"EmailVerificationRoute"` or `"PasswordResetRoute"`).

The route must be defined in [App.spec](#spec) with the [route](#route-1) constructor.

This route should handle the process of taking a token from the URL and sending it to the server to verify the e-mail address. You can use our [`verifyEmail` action](https://wasp.sh/docs/features/auth/email/create-your-own-ui#verifyemail) and [`resetPassword` action](https://wasp.sh/docs/features/auth/email/create-your-own-ui#resetpassword) helpers for that.

##### getEmailContentFn?

> `optional` **getEmailContentFn?**: [`Reference`](#reference)<[`AnyFunction`](#anyfunction)>

Function that returns the email content (subject, html, text) Wasp sends for this flow. Verification email functions receive `{ verificationLink }`, and password reset email functions receive `{ passwordResetLink }`. If omitted, Wasp uses a built-in default template.

In TypeScript, you can type the function with the `GetVerificationEmailContentFn` or `GetPasswordResetEmailContentFn` type from `wasp/server/auth`.

###### Example

```ts title="src/email.ts"
import { GetVerificationEmailContentFn } from "wasp/server/auth"

export const getVerificationEmailContent: GetVerificationEmailContentFn = ({
  verificationLink,
}) => ({
  subject: "Verify your email",
  text: `Click the link below to verify your email: ${verificationLink}`,
  html: `
        <p>Click the link below to verify your email</p>
        <a href="${verificationLink}">Verify email</a>
    `,
})
```

---

### EmailFromField

Sender identity used in outgoing emails.

#### Properties

##### email

> **email**: `string`

Email address the message is sent from.

##### name? {#name-2}

> `optional` **name?**: `string`

Optional display name shown alongside the email address.

---

### EmailSender {#emailsender-1}

Email sender configuration.

Required for the email auth flows (verification, password reset) and available for sending arbitrary emails via `wasp/server/email`.

See [Sending Emails](https://wasp.sh/docs/features/email).

#### Example

```ts
import { app } from "@wasp/spec"

export default app({
  // ...
  emailSender: {
    provider: "SMTP",
    defaultFrom: {
      name: "Example",
      email: "hello@itsme.com",
    },
  },
})
```

#### Properties

##### defaultFrom?

> `optional` **defaultFrom?**: [`EmailFromField`](#emailfromfield)

The default sender's details. If you set this field, you don't need to provide the `from` field when sending an email.

##### provider

> **provider**: [`EmailSenderProviderName`](#emailsenderprovidername)

Provider Wasp uses to deliver outgoing emails.

---

### ExecutorOptions

Executor-specific options.

The `pgBoss` field is forwarded as-is to the matching pg-boss API. See the [pg-boss send options docs](https://github.com/timgit/pg-boss/blob/8.4.2/docs/readme.md#sendname-data-options) for the supported keys.

#### Properties

##### pgBoss

> **pgBoss**: `object`

---

### ExternalAuthMethods

Social (OAuth) auth methods.

Each enabled provider also requires the matching client ID and secret to be set as environment variables. See the [Social Auth overview](https://wasp.sh/docs/features/auth/social-auth/overview) for details on each provider.

#### Extends

- `Partial`<`Record`<[`SocialAuthMethodName`](#socialauthmethodname), [`SocialAuthConfig`](#socialauthconfig)>>

#### Properties

##### discord?

> `optional` **discord?**: [`SocialAuthConfig`](#socialauthconfig)

###### Inherited from

[`ExternalAuthMethods`](#externalauthmethods).[`discord`](#discord)

##### gitHub?

> `optional` **gitHub?**: [`SocialAuthConfig`](#socialauthconfig)

###### Inherited from

[`ExternalAuthMethods`](#externalauthmethods).[`gitHub`](#github)

##### google?

> `optional` **google?**: [`SocialAuthConfig`](#socialauthconfig)

###### Inherited from

[`ExternalAuthMethods`](#externalauthmethods).[`google`](#google)

##### keycloak?

> `optional` **keycloak?**: [`SocialAuthConfig`](#socialauthconfig)

###### Inherited from

[`ExternalAuthMethods`](#externalauthmethods).[`keycloak`](#keycloak)

##### microsoft?

> `optional` **microsoft?**: [`SocialAuthConfig`](#socialauthconfig)

###### Inherited from

[`ExternalAuthMethods`](#externalauthmethods).[`microsoft`](#microsoft)

##### slack?

> `optional` **slack?**: [`SocialAuthConfig`](#socialauthconfig)

###### Inherited from

[`ExternalAuthMethods`](#externalauthmethods).[`slack`](#slack)

---

### LocalAuthMethods

Built-in local auth methods.

Pick at most one of these.

#### Properties

##### email

> **email**: [`EmailAuthConfig`](#emailauthconfig)

Enables email and password auth with email verification and password reset flows.

##### usernameAndPassword

> **usernameAndPassword**: [`UsernameAndPasswordConfig`](#usernameandpasswordconfig)

Enables username and password auth.

---

### Schedule {#schedule-1}

Cron schedule for a [Job](#job).

#### Properties

##### args?

> `optional` **args?**: `object`

Arguments passed to the worker function on each scheduled run.

##### cron

> **cron**: `string`

Five-field cron expression (e.g. `"0 * * * *"` for hourly).

Wasp supports minute-level precision.

See pg-boss's [scheduling docs](https://github.com/timgit/pg-boss/blob/8.4.2/docs/readme.md#scheduling) for the rationale. Use [Crontab Guru](https://crontab.guru/#0_*_*_*_*) to build cron expressions.

##### executorOptions?

> `optional` **executorOptions?**: [`ExecutorOptions`](#executoroptions)

Executor-specific options applied only to scheduled runs. These override or extend [Job.performExecutorOptions](#performexecutoroptions).

---

### Server {#server-1}

Server-side application configuration.

See [Server Config](https://wasp.sh/docs/advanced/server-customization/server-config) for usage details.

#### Example

```ts
import { app } from "@wasp/spec"
import { myMiddlewareConfigFn, mySetupFunction } from "./src/myServerSetupCode" with { type: "ref" }

export default app({
  // ...
  server: {
    setupFn: mySetupFunction,
    middlewareConfigFn: myMiddlewareConfigFn,
  },
})
```

#### Properties

##### envValidationSchema?

> `optional` **envValidationSchema?**: [`Reference`](#reference)<{ `_zod`: { `def`: `object`; }; }>

Zod schema used to validate user-defined server environment variables on startup. Wasp merges it with built-in validation for Wasp-defined env vars when validating `process.env`. Define the schema with `defineEnvValidationSchema` from `wasp/env` to make sure it is type-checked.

Use [reference imports](https://wasp.sh/docs/features/spec#reference-imports) when passing values from your app into the Wasp spec. If a direct import is not practical, use [ref](#ref) as a fallback.

For example:

```ts
import { serverEnvSchema } from './src/env' with { type: 'ref' }
```

See [Env Vars](https://wasp.sh/docs/advanced/env-vars).

##### middlewareConfigFn? {#middlewareconfigfn-2}

> `optional` **middlewareConfigFn?**: [`Reference`](#reference)<[`AnyFunction`](#anyfunction)>

Function that customizes the global Express middleware stack. Affects all operations and APIs.

See [Configuring Middleware](https://wasp.sh/docs/advanced/server-customization/middleware).

##### setupFn?

> `optional` **setupFn?**: [`Reference`](#reference)<[`AnyFunction`](#anyfunction)>

Async function called once on server start. Wasp awaits it before the server accepts requests. Receives a context containing the Express `app` and underlying `http.Server`, so you can register custom routes, set up additional databases or WebSockets, or kick off scheduled jobs.

In TypeScript, you can type the function with the `ServerSetupFn` type from `wasp/server`.

###### Example

```ts
import { type ServerSetupFn } from "wasp/server"

export const mySetupFunction: ServerSetupFn = async () => {
  await setUpSomeResource()
}
```

##### traceDeps?

> `optional` **traceDeps?**: `string`\[]

Extra packages to copy into the server's Docker image.

The Docker image contains only the files the server imports. A package that loads files in a non-standard way (for example, static assets it serves from its own folder) can miss some of them. Each entry is a package name with an optional prefix or suffix, the same syntax as Nitro's `traceDeps`:

- `"pkg"` traces the package even when the server doesn't import it.
- `"pkg*"` copies all of the package's files.
- `"!pkg"` stops copying all files of a package Wasp copies fully by default.

We use [nf3](https://github.com/unjs/nf3), which finds most of the files your dependencies need by default. Only use this setting if you've confirmed a necessary file is missing.

###### Example

```ts
traceDeps: ["swagger-ui-dist*"]
```

###### See

- <https://wasp.sh/docs/deployment/methods/overview#missing-files-in-the-docker-image>
- <https://nitro.build/config#tracedeps> for the entry syntax.
- <https://github.com/unjs/nf3/blob/main/src/db.ts> for the packages copied in full by default.

---

### SocialAuthConfig

Social auth provider configuration.

By default, Wasp stores only the user's provider-specific ID. Use `userSignupFields` to save provider account details or custom signup state on your [Auth.userEntity](#userentity). Each provider has provider-specific `userSignupFields` and `configFn` behavior, documented in the [Social Auth docs](https://wasp.sh/docs/features/auth/social-auth/overview).

#### Example

```ts
import { app } from "@wasp/spec"
import { getConfig, userSignupFields } from "./src/auth/google" with { type: "ref" }

export default app({
  // ...
  auth: {
    userEntity: "User",
    methods: {
      google: {
        configFn: getConfig,
        userSignupFields,
      },
    },
    onAuthFailedRedirectTo: "/login",
  },
})
```

#### Extends

- [`BaseAuthMethodConfig`](#baseauthmethodconfig)

#### Properties

##### configFn?

> `optional` **configFn?**: [`Reference`](#reference)<[`AnyFunction`](#anyfunction)>

Function that returns provider-specific OAuth options (scopes, prompt, etc.). Use this to request extra scopes, such as requesting email/profile data from providers that do not include it by default, or to customize the OAuth flow.

###### Example

```ts
export function getConfig() {
  return {
    scopes: ["openid", "profile", "email"],
  }
}
```

##### userSignupFields?

> `optional` **userSignupFields?**: [`Reference`](#reference)<[`AnyObject`](#anyobject)>

Object that defines extra fields to save on the user during signup (e.g. `firstName`, `address`). Each field name must exist on the configured [Auth.userEntity](#userentity).

Each field function receives the data sent from the client and returns the value Wasp saves to the database. For social auth, this data includes provider-specific profile information. If the value is invalid, throw an error. The `password` field is excluded from this object and handled by Wasp's auth backend.

See [Signup Fields Customization](https://wasp.sh/docs/features/auth/overview#signup-fields-customization).

###### Example

```ts
import { defineUserSignupFields } from 'wasp/server/auth'

export const userSignupFields = defineUserSignupFields({
  address: (data) => {
    if (!data.address) {
      throw new Error('Address is required')
    }
    return data.address
  },
  phone: (data) => data.phone,
})
```

###### Inherited from

[`BaseAuthMethodConfig`](#baseauthmethodconfig).[`userSignupFields`](#usersignupfields)

---

### UsernameAndPasswordConfig

Username and password auth configuration.

#### Example

```ts
import { app } from "@wasp/spec"
import { userSignupFields } from "./src/auth" with { type: "ref" }

export default app({
  // ...
  auth: {
    userEntity: "User",
    methods: {
      usernameAndPassword: {
        userSignupFields,
      },
    },
    onAuthFailedRedirectTo: "/login",
  },
})
```

#### Extends

- [`BaseAuthMethodConfig`](#baseauthmethodconfig)

#### Properties

##### userSignupFields?

> `optional` **userSignupFields?**: [`Reference`](#reference)<[`AnyObject`](#anyobject)>

Object that defines extra fields to save on the user during signup (e.g. `firstName`, `address`). Each field name must exist on the configured [Auth.userEntity](#userentity).

Each field function receives the data sent from the client and returns the value Wasp saves to the database. For social auth, this data includes provider-specific profile information. If the value is invalid, throw an error. The `password` field is excluded from this object and handled by Wasp's auth backend.

See [Signup Fields Customization](https://wasp.sh/docs/features/auth/overview#signup-fields-customization).

###### Example

```ts
import { defineUserSignupFields } from 'wasp/server/auth'

export const userSignupFields = defineUserSignupFields({
  address: (data) => {
    if (!data.address) {
      throw new Error('Address is required')
    }
    return data.address
  },
  phone: (data) => data.phone,
})
```

###### Inherited from

[`BaseAuthMethodConfig`](#baseauthmethodconfig).[`userSignupFields`](#usersignupfields)

---

### Wasp {#wasp-1}

Wasp compiler metadata used by the app.

#### Properties

##### version

> **version**: `string`

The Wasp version this app is built for, as an npm-compatible [SemVer range](https://github.com/npm/node-semver#ranges) (e.g. `"^0.24.0"`).

---

### WebSocket {#websocket-1}

WebSocket configuration.

See [Web Sockets](https://wasp.sh/docs/features/websockets) for handler shape and client-side usage.

#### Example

```ts
import { app } from "@wasp/spec"
import { webSocketFn } from "./src/webSocket" with { type: "ref" }

export default app({
  // ...
  webSocket: {
    fn: webSocketFn,
    autoConnect: true,
  },
})
```

#### Properties

##### autoConnect?

> `optional` **autoConnect?**: `boolean`

If `true` (the default), the client connects to the WebSocket server as soon as the app loads. Set to `false` to connect manually via `useSocket`.

###### Default

```ts
true
```

##### fn {#fn-4}

> **fn**: [`Reference`](#reference)<[`AnyFunction`](#anyfunction)>

Function that registers Socket.IO event handlers. Wasp calls it once on server start with the Socket.IO server instance and a context containing all app entities. If a connected socket is authenticated, Wasp stores the user on `socket.data.user`.

See [the `websocketFn` docs](https://wasp.sh/docs/features/websockets#websocketfn).

---

### ActionConfig

> **ActionConfig** = `Omit`<[`Action`](#action), `"kind"` | `"fn"`>

The optional configuration object accepted as the last argument of the [action](#action-1) constructor.

---

### AnyFunction

> **AnyFunction** = (...`args`) => `unknown`

#### Parameters

##### args

...`never`\[]

#### Returns

`unknown`

---

### AnyObject

> **AnyObject** = `Record`<`PropertyKey`, `unknown`>

---

### ApiConfig

> **ApiConfig** = `Omit`<[`Api`](#api), `"kind"` | `"method"` | `"path"` | `"fn"`>

The optional configuration object accepted as the last argument of the [api](#api-1) constructor.

---

### ApiNamespaceConfig

> **ApiNamespaceConfig** = `Omit`<[`ApiNamespace`](#apinamespace), `"kind"` | `"path"`>

The configuration object accepted as the last argument of the [apiNamespace](#apinamespace-1) constructor.

---

### AuthMethods {#authmethods-1}

> **AuthMethods** = `RequireOneOrNone`<[`LocalAuthMethods`](#localauthmethods)> & [`ExternalAuthMethods`](#externalauthmethods)

Enabled authentication methods for the app.

At most one local auth method ([LocalAuthMethods](#localauthmethods)) can be used, but you can enable as many external auth methods ([ExternalAuthMethods](#externalauthmethods)) as you need.

---

### Brand

> **Brand**<`B`> = `object`

#### Type Parameters

##### B

`B`

#### Properties

##### \[\_\_\_brand]

> **\[\_\_\_brand]**: `B`

---

### Branded

> **Branded**<`T`, `B`> = `T` & [`Brand`](#brand)<`B`>

#### Type Parameters

##### T

`T`

##### B

`B`

---

### DeploymentMode {#deploymentmode-1}

> **DeploymentMode** = `"split"`

Supported app deployment modes.

---

### EmailSenderProviderName

> **EmailSenderProviderName** = `"SMTP"` | `"SendGrid"` | `"Mailgun"` | `"Resend"` | `"Dummy"`

Supported email sender providers.

`"Dummy"` logs emails to the console instead of sending them, which is convenient during development, but not allowed in production.

---

### Entities {#entities-4}

> **Entities** = [`FromRegister`](#fromregister)<`"entities"`, { }>

---

### EntityName

> **EntityName** = keyof [`Entities`](#entities-4) *extends* `never` ? `string` : `Extract`\<keyof [`Entities`](#entities-4), `string`>

One of the app's entity names, matching a model in `schema.prisma`.

---

### FromRegister

> **FromRegister**<`Key`, `Fallback`> = `Key` *extends* keyof `Register` ? `Register`\[`Key`] : `Fallback`

#### Type Parameters

##### Key

`Key` *extends* `string`

##### Fallback

`Fallback`

---

### HttpMethod

> **HttpMethod** = `"ALL"` | `"GET"` | `"POST"` | `"PUT"` | `"PATCH"` | `"DELETE"` | `"HEAD"`

HTTP methods supported by an [Api](#api).

Use `"ALL"` to match any method.

A `"HEAD"` api must be declared before a `"GET"` api on the same path, otherwise the `"GET"` api answers the HEAD requests as well.

---

### JobConfig

> **JobConfig** = `Omit`<[`Job`](#job), `"kind"` | `"fn"`>

The configuration object accepted as the last argument of the [job](#job-1) constructor.

---

### JobExecutor {#jobexecutor-1}

> **JobExecutor** = `"PgBoss"`

Supported job executors.

Currently only `"PgBoss"` is available, which stores jobs in the app's PostgreSQL database via [pg-boss](https://github.com/timgit/pg-boss).

---

### PageConfig

> **PageConfig** = `Omit`<[`Page`](#page), `"kind"` | `"component"`>

The optional configuration object accepted as the last argument of the [page](#page-2) constructor.

---

### QueryConfig

> **QueryConfig** = `Omit`<[`Query`](#query), `"kind"` | `"fn"`>

The optional configuration object accepted as the last argument of the [query](#query-1) constructor.

---

### RouteConfig

> **RouteConfig** = `Omit`<[`Route`](#route), `"kind"` | `"name"` | `"path"` | `"page"`>

The optional configuration object accepted as the last argument of the [route](#route-1) constructor.

---

### SocialAuthMethodName

> **SocialAuthMethodName** = `"discord"` | `"google"` | `"gitHub"` | `"keycloak"` | `"microsoft"` | `"slack"`

Supported social auth providers.