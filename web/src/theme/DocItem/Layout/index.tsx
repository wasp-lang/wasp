import { useDoc } from "@docusaurus/plugin-content-docs/client";
import type { WrapperProps } from "@docusaurus/types";
import useBaseUrl from "@docusaurus/useBaseUrl";
import useDocusaurusContext from "@docusaurus/useDocusaurusContext";
import Giscus from "@giscus/react";
import Layout from "@theme-original/DocItem/Layout";
import type LayoutType from "@theme/DocItem/Layout";
import { type ReactNode } from "react";

type Props = WrapperProps<typeof LayoutType>;

export default function LayoutWrapper(props: Props): ReactNode {
  const doc = useDoc();

  const unversionedId = "docs/" + doc.metadata.id;
  const absolutePathToUnversionedId = useBaseUrl(unversionedId, {
    absolute: true,
    forcePrependBaseUrl: true,
  });
  const backlink = new URL(
    absolutePathToUnversionedId,
    useDocusaurusContext().siteConfig.url,
  );

  return (
    <>
      <Layout {...props} />

      {doc.frontMatter.comments ? (
        <>
          <p />
          <meta name="giscus:backlink" content={backlink.toString()} />
          <Giscus
            repo="wasp-lang/wasp.sh-comments"
            repoId="R_kgDOQ2GLxw"
            category="Comments"
            categoryId="DIC_kwDOQ2GLx84C0uT8"
            mapping="specific"
            term={"docs/" + doc.metadata.id}
            strict="1"
            reactionsEnabled="0"
            inputPosition="top"
            theme="preferred_color_scheme"
            lang="en"
            loading="lazy"
          />
        </>
      ) : null}
    </>
  );
}
