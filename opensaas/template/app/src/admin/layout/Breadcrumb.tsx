import { Link as WaspRouterLink, routes } from "wasp/client/router";
interface BreadcrumbProps {
  pageName: string;
}
export function Breadcrumb({ pageName }: BreadcrumbProps) {
  return (
    <div className="mb-6 flex flex-col gap-3 sm:flex-row sm:items-center sm:justify-between">
      <h2 className="text-title-md2 text-foreground font-semibold">
        {pageName}
      </h2>

      <nav>
        <ul className="flex items-center gap-1">
          <li>
            <WaspRouterLink to={routes.AdminRoute.to}>Dashboard</WaspRouterLink>
          </li>
          <li>/</li>
          <li className="font-medium">{pageName}</li>
        </ul>
      </nav>
    </div>
  );
}
