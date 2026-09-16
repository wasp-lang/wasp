import { DefaultRootErrorBoundary } from './components/DefaultRootErrorBoundary';
import { routes } from '../router/index';
export function getRouteObjects({ routesMapping, rootElement, }) {
    const waspDefinedRoutes = [];
    const userDefinedRoutes = Object.entries(routes).map(([routeKey, route]) => {
        return {
            path: route.to,
            ...routesMapping[routeKey],
        };
    });
    return [{
            path: '/',
            element: rootElement,
            ErrorBoundary: DefaultRootErrorBoundary,
            children: [
                ...waspDefinedRoutes,
                ...userDefinedRoutes,
            ],
        }];
}
//# sourceMappingURL=router.jsx.map