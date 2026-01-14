export function getRoutesTsxContent(): string {
  return `
  import { MainPage } from './src/MainPage'

  export const routesMapping = {
    RootRoute: MainPage,
  } as const;
`
}
