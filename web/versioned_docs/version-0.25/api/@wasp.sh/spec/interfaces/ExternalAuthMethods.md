# Interface: ExternalAuthMethods

Social (OAuth) auth methods.

Each enabled provider also requires the matching client ID and secret to
be set as environment variables. See the
[Social Auth overview](https://wasp.sh/docs/auth/social-auth/overview) for
details on each provider.

## Extends

- `Partial`\<`Record`\<`SocialAuthMethodName`, [`SocialAuthConfig`](SocialAuthConfig.md)\>\>

## Properties

### discord?

> `optional` **discord?**: [`SocialAuthConfig`](SocialAuthConfig.md)

#### Inherited from

`ExternalAuthMethods`.[`discord`](#discord)

***

### gitHub?

> `optional` **gitHub?**: [`SocialAuthConfig`](SocialAuthConfig.md)

#### Inherited from

`ExternalAuthMethods`.[`gitHub`](#github)

***

### google?

> `optional` **google?**: [`SocialAuthConfig`](SocialAuthConfig.md)

#### Inherited from

`ExternalAuthMethods`.[`google`](#google)

***

### keycloak?

> `optional` **keycloak?**: [`SocialAuthConfig`](SocialAuthConfig.md)

#### Inherited from

`ExternalAuthMethods`.[`keycloak`](#keycloak)

***

### microsoft?

> `optional` **microsoft?**: [`SocialAuthConfig`](SocialAuthConfig.md)

#### Inherited from

`ExternalAuthMethods`.[`microsoft`](#microsoft)

***

### slack?

> `optional` **slack?**: [`SocialAuthConfig`](SocialAuthConfig.md)

#### Inherited from

`ExternalAuthMethods`.[`slack`](#slack)
