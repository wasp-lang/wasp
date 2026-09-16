import type { ReactNode } from 'react';
import type { RouteObject } from 'react-router';
type RouteMapping = Record<string, RouteObject>;
export declare function getRouteObjects({ routesMapping, rootElement, }: {
    routesMapping: RouteMapping;
    rootElement: ReactNode;
}): RouteObject[];
export {};
//# sourceMappingURL=router.d.ts.map