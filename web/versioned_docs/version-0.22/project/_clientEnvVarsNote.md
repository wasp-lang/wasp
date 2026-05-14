:::caution Client Env Var Prefix

Client env vars must be prefixed with `REACT_APP_`, for example: `REACT_APP_SOME_VAR_NAME=...` for security reasons. Wasp will only inject env vars that start with this prefix into the client code to prevent accidental exposure of sensitive information.
:::
