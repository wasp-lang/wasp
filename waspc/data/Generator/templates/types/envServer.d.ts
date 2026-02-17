{{={= =}=}}
{=# envValidationSchema.isDefined =}
{=& envValidationSchema.importStatement =}

declare module 'wasp/types' {
  interface Register {
    serverEnvSchema: typeof {= envValidationSchema.importIdentifier =}
  }
}
{=/ envValidationSchema.isDefined =}
