# Interface: Api

A custom HTTP endpoint mounted on the Wasp server.

Create one with the [api](../functions/api.md) constructor.

Unlike operations, APIs are plain Express handlers that receive `req`,
`res`, and a Wasp `context`. They are useful for webhooks and any
non-standard HTTP interaction.

See [Custom HTTP API Endpoints](https://wasp.sh/docs/advanced/apis).

## Extends

- `BaseSpecElement`\<`"api"`\>

## Properties

### auth?

> `optional` **auth?**: `boolean`

If `true`, the handler requires the request to come from an
authenticated user and receives `context.user`. Defaults to `true` when
the app has auth enabled, and `false` otherwise. Set to `false` to skip
parsing the JWT from the Authorization header.

***

### entities?

> `optional` **entities?**: `string`[]

Entities the handler operates on. Wasp injects a Prisma delegate for
each one into the handler's `context.entities`.

See [Using Entities in APIs](https://wasp.sh/docs/advanced/apis#using-entities-in-apis).

***

### fn

> **fn**: [`Reference`](../type-aliases/Reference.md)\<`AnyFunction`\>

Reference to the API's NodeJS implementation.

***

### kind

> **kind**: `"api"`

The internal Wasp type of a [SpecElement](../type-aliases/SpecElement.md). Used by the compiler.
You should not set this field directly, instead use the dedicated constructors.

#### Inherited from

`BaseSpecElement.kind`

***

### method

> **method**: [`HttpMethod`](../type-aliases/HttpMethod.md)

HTTP method this endpoint responds to.

***

### middlewareConfigFn?

> `optional` **middlewareConfigFn?**: [`Reference`](../type-aliases/Reference.md)\<`AnyFunction`\>

Reference to an Express middleware config function for this endpoint only.

See [Configuring API middleware](https://wasp.sh/docs/advanced/middleware-config#2-customize-api-specific-middleware).

***

### path

> **path**: `string`

Express path of the endpoint (e.g. `"/webhooks/stripe"`).
