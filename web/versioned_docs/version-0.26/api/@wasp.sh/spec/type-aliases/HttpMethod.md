# Type Alias: HttpMethod

> **HttpMethod** = `"ALL"` \| `"GET"` \| `"POST"` \| `"PUT"` \| `"PATCH"` \| `"DELETE"` \| `"HEAD"`

HTTP methods supported by an [Api](../interfaces/Api.md).

Use `"ALL"` to match any method.

A `"HEAD"` api must be declared before a `"GET"` api on the same path,
otherwise the `"GET"` api answers the HEAD requests as well.
