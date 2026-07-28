# Type Alias: AuthMethods

> **AuthMethods** = `RequireOneOrNone`\<[`LocalAuthMethods`](../interfaces/LocalAuthMethods.md)\> & [`ExternalAuthMethods`](../interfaces/ExternalAuthMethods.md)

Enabled authentication methods for the app.

At most one local auth method ([LocalAuthMethods](../interfaces/LocalAuthMethods.md)) can be used, but you
can enable as many external auth methods ([ExternalAuthMethods](../interfaces/ExternalAuthMethods.md)) as you
need.
