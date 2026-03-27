// @ts-nocheck

export const routesMapping = {
  RootRoute: { lazy: async () => {
    const Component = await import('./src/MainPage').then(m => m.MainPage)
    return { Component }
  }},
} as const;
