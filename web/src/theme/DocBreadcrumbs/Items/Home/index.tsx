import React, { type ReactNode } from "react";
import Link from "@docusaurus/Link";
import useBaseUrl from "@docusaurus/useBaseUrl";

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
