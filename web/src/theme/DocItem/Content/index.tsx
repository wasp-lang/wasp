import { useDoc } from "@docusaurus/plugin-content-docs/client";
import type { WrapperProps } from "@docusaurus/types";
import DatedContent from "@site/src/components/DatedContent";
import Content from "@theme-original/DocItem/Content";
import type ContentType from "@theme/DocItem/Content";
import { type ReactNode } from "react";

type Props = WrapperProps<typeof ContentType>;

export default function ContentWrapper(props: Props): ReactNode {
  const doc = useDoc();

  return (
    <>
      {doc.frontMatter.last_update ? (
        <>
          <p />
          <DatedContent frontMatter={doc.frontMatter} />
        </>
      ) : null}

      <Content {...props} />
    </>
  );
}
