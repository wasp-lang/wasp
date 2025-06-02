import Details from "@theme/MDXComponents/Details";
import React from "react";

export default function Collapse(props: {
  children: React.ReactNode;
  title: string;
}) {
  const { children, title } = props;
  return (
    <Details>
      <summary mdxType="summary">{title}</summary>
      {children}
    </Details>
  );
}
