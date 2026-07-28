# Interface: Page

A page in the app, normally a React component rendered for a [Route](Route.md).

Create one with the [page](../functions/page.md) constructor.

## Extends

- `BaseSpecElement`\<`"page"`\>

## Properties

### authRequired?

> `optional` **authRequired?**: `boolean`

If `true`, only authenticated users can access this page. Unauthenticated
visitors are redirected to [Auth.onAuthFailedRedirectTo](Auth.md#onauthfailedredirectto).

Cannot be combined with [Route.prerender](Route.md#prerender).

#### Default

```ts
false
```

***

### component

> **component**: [`Reference`](../type-aliases/Reference.md)\<`AnyFunction`\>

React component rendered for this page.

***

### kind

> **kind**: `"page"`

The internal Wasp type of a [SpecElement](../type-aliases/SpecElement.md). Used by the compiler.
You should not set this field directly, instead use the dedicated constructors.

#### Inherited from

`BaseSpecElement.kind`
