// @ts-nocheck

export const routesMapping = {
  RootRoute: async () => {
    const { MainPage: Component } = await import('./src/MainPage')
    return { Component }
  },
} as const;
