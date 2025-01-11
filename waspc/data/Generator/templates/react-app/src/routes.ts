import {
  type RouteConfig,
  route,
} from "@react-router/dev/routes";
import { layout, route } from "@react-router/dev/routes";
import { routes } from 'wasp/client/router'

export const routeNameToRouteComponent = {
  {=# routes =}
  {= name =}: {= targetComponent =},
  {=/ routes =}
} as const;

const waspDefinedRoutes = [
  {=# isExternalAuthEnabled =}
  route("{= oAuthCallbackPath =}", "./auth/pages/OAuthCallback"),
  {=/ isExternalAuthEnabled =}
]
const userDefinedRoutes = Object.entries(routes).map(([routeKey, route]) => {
  // TODO: This should be the path to the route module file (eg. /src/routes/home.tsx)
  return route(routeKey, route)
 
})


export default [
  layout(
    // TODO: This should maybe always be defined, if not, 
    // TODO: the code should be (if defined => layout, otherwise => spread the routes normally in the array)
    {=# rootComponent.isDefined =}
    "{= rootComponent.importIdentifier =}"
    {=/ rootComponent.isDefined =}, 
    [...waspDefinedRoutes, ...userDefinedRoutes]
  ), 
] satisfies RouteConfig;