import Admonition from "@theme/Admonition";
import Link from "@docusaurus/Link";
import React from "react";

export default function OldDocsNote() {
  return (
    <div
      style={{
        position: "sticky",
        top: "calc(var(--ifm-navbar-height) + 1rem)",
        zIndex: 1,
      }}
    >
      <Admonition type="caution" title="Deprecated Page">
        This page is part of a previous documentation version and is no longer
        actively maintained. The content is likely out of date and may no longer
        be relevant to current releases.
        <br />
        <br />
        Go to the <Link to="/docs">current documentation</Link> for updated
        content.
      </Admonition>
    </div>
  );
}
