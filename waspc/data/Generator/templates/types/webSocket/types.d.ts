{{={= =}=}}
{=& webSocketFn.importStatement =}

declare module 'wasp/types' {
  interface Register {
    webSocketFn: typeof {= webSocketFn.importIdentifier =}
  }
}
