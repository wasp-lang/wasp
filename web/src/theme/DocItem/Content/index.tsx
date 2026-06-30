import { useDoc } from "@docusaurus/plugin-content-docs/client";
import { ThemeClassNames } from "@docusaurus/theme-common";
import type { WrapperProps } from "@docusaurus/types";
import LastCheckedWithVersionsNotice, {
  LastCheckedWithVersions,
} from "@site/src/components/VersionNotice";
import type ContentType from "@theme/DocItem/Content";
import Heading from "@theme/Heading";
import MDXContent from "@theme/MDXContent";
import clsx from "clsx";
import type { ReactNode } from "react";

type Props = WrapperProps<typeof ContentType>;

interface FrontMatter {
  hide_title?: boolean;
  last_checked_with_versions?: LastCheckedWithVersions;
}

/**
 * Ejected and modified Docusaurus `DocItem/Content` component.
 * Modifications:
 * - Adds the "Last checked with versions" notice inside the markdown container, but
 *   before any other content. The original component only lets us inject the notice
 *   as `<MDXContent>`'s child, which always renders it below the frontmatter title.
 *
 * @see {@link https://docusaurus.io/docs/swizzling Docasaurus swizzling}
 */
export default function Content({ children }: Props): ReactNode {
  const frontMatter = useDoc().frontMatter as FrontMatter;
  const syntheticTitle = useSyntheticTitle();

  return (
    <div className={clsx(ThemeClassNames.docs.docMarkdown, "markdown")}>
      {frontMatter.last_checked_with_versions && (
        <LastCheckedWithVersionsNotice
          lastCheckedWithVersions={frontMatter.last_checked_with_versions}
        />
      )}

      {syntheticTitle && (
        <header>
          <Heading as="h1">{syntheticTitle}</Heading>
        </header>
      )}

      <MDXContent>{children}</MDXContent>
    </div>
  );
}

function useSyntheticTitle(): string | null {
  const { metadata, frontMatter, contentTitle } = useDoc();
  const shouldRender =
    !frontMatter.hide_title && typeof contentTitle === "undefined";
  if (!shouldRender) {
    return null;
  }
  return metadata.title;
}
