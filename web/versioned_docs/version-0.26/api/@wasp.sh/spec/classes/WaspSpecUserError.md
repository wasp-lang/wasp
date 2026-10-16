# Class: WaspSpecUserError

Error caused by invalid user input (unknown entities, bad imports, etc.).

Wasp's spec pipeline throws `WaspSpecUserError` when it finds a problem with the
user's configuration. Its message is reported to the user as a clean,
actionable error instead of an internal stack trace.

Throw it from your own spec helper libraries when you want to report a
configuration problem the same way Wasp does:

## Example

```ts
import { WaspSpecUserError } from "@wasp.sh/spec"

export function requireEnv(name: string): string {
  const value = process.env[name]
  if (value === undefined) {
    throw new WaspSpecUserError(`Missing required environment variable '${name}'.`)
  }
  return value
}
```

These are the errors users hit most often during development, so keep their
messages short, clear, and actionable.

## Extends

- `Error`

## Constructors

### Constructor

> **new WaspSpecUserError**(`message?`): `WaspSpecUserError`

#### Parameters

##### message?

`string`

#### Returns

`WaspSpecUserError`

#### Inherited from

`Error.constructor`

### Constructor

> **new WaspSpecUserError**(`message?`, `options?`): `WaspSpecUserError`

#### Parameters

##### message?

`string`

##### options?

`ErrorOptions`

#### Returns

`WaspSpecUserError`

#### Inherited from

`Error.constructor`
