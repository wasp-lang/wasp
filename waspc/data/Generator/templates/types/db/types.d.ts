{{={= =}=}}
{=# prismaSetupFn.isDefined =}
{=& prismaSetupFn.importStatement =}

declare module 'wasp/types' {
  interface Register {
    prismaSetupFn: typeof {= prismaSetupFn.importIdentifier =}
  }
}
{=/ prismaSetupFn.isDefined =}
