import Head from "@docusaurus/Head";
import {
  useDoc,
  useDocsVersion,
  useLatestVersion,
} from "@docusaurus/plugin-content-docs/client";
import type { WrapperProps } from "@docusaurus/types";
import { useBaseUrlUtils } from "@docusaurus/useBaseUrl";
import { getLatestDocCanonicalPath } from "@site/src/lib/old-docs-versions";
import DocItemMetadata from "@theme-original/DocItem/Metadata";
import type DocItemMetadataType from "@theme/DocItem/Metadata";
import { type ReactNode } from "react";

type Props = WrapperProps<typeof DocItemMetadataType>;

export default function DocItemMetadataWrapper(props: Props): ReactNode {
  const latestDocCanonicalUrl = useLatestDocCanonicalUrl();

  return (
    <>
      <DocItemMetadata {...props} />
      {latestDocCanonicalUrl && (
        <Head>
          <link rel="canonical" href={latestDocCanonicalUrl} />
        </Head>
      )}
    </>
  );
}

// The canonical link here replaces the self-referencing one that Docusaurus
// adds to every page.
function useLatestDocCanonicalUrl(): string | undefined {
  const { metadata } = useDoc();
  const { pluginId } = useDocsVersion();
  const latestVersion = useLatestVersion(pluginId);
  const { withBaseUrl } = useBaseUrlUtils();

  const latestPath = getLatestDocCanonicalPath(
    metadata.permalink,
    new Set(latestVersion.docs.map((doc) => doc.path)),
  );

  return latestPath && withBaseUrl(latestPath, { absolute: true });
}
