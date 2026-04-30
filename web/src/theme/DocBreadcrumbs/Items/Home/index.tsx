import Link from "@docusaurus/Link";
import useBaseUrl from "@docusaurus/useBaseUrl";
import { type ReactNode } from "react";

export default function HomeBreadcrumbItem(): ReactNode {
  const homeHref = useBaseUrl("/docs");

  return (
    <li className="breadcrumbs__item">
      <Link className="breadcrumbs__link" href={homeHref}>
        Docs
      </Link>
    </li>
  );
}
