import { useDoc } from "@docusaurus/plugin-content-docs/client";
import { useColorMode } from "@docusaurus/theme-common";
import useBaseUrl from "@docusaurus/useBaseUrl";
import useDocusaurusContext from "@docusaurus/useDocusaurusContext";
import Giscus from "@giscus/react";
import type { ReactNode } from "react";

export default function DocComments(): ReactNode {
  const doc = useDoc();
  const { colorMode } = useColorMode();

  // We reconstruct the unversioned doc ID, because Giscus discussions
  // should be shared between different versions of the same doc.
  const unversionedDocId = "docs/" + doc.metadata.id;
  const unversionedDocAbsPath = useBaseUrl(unversionedDocId, {
    absolute: true,
    forcePrependBaseUrl: true,
  });
  const unversionedDocUrl = new URL(
    unversionedDocAbsPath,
    useDocusaurusContext().siteConfig.url,
  ).toString();

  return (
    <>
      {/*
        This property allows Giscus to set a canonical backlink for the discussion.
        e.g. if a discussion is first created by a user on a versioned doc URL,
        its link will point to the unversioned doc URL instead of the archived one.

        React auto-moves <meta> tags to the document head.
        https://react.dev/reference/react-dom/components/meta
      */}
      <meta name="giscus:backlink" content={unversionedDocUrl} />

      <Giscus
        // Repo and discussion category where to store comments.
        repo="wasp-lang/wasp.sh-comments"
        repoId="R_kgDOQ2GLxw"
        category="Comments"
        categoryId="DIC_kwDOQ2GLx84C0uT8"
        // Mapping each page to a specific discussion:
        // We use "specific" + strict mapping, so Giscus will look for a discussion
        // with the exact term we provide. (Or create it if it doesn't exist yet.)
        mapping="specific"
        strict="1"
        term={unversionedDocId}
        // Some other look-and-feel options.
        lang="en"
        reactionsEnabled="0"
        inputPosition="top"
        theme={colorMode}
        // Only loads when the user scrolls near the comments.
        loading="lazy"
      />
    </>
  );
}
