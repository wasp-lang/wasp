{{={= =}=}}
{=# envValidationSchema.isDefined =}
{=& envValidationSchema.importStatement =}

declare module 'wasp/types' {
  interface Register {
    clientEnvSchema: typeof {= envValidationSchema.importIdentifier =}
  }
}
{=/ envValidationSchema.isDefined =}
