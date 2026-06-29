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
 * Reimplements Docusaurus's original `DocItemContent` so the "Last checked
 * with versions" notice can live inside the `markdown` container, above the
 * title.
 *
 * The original component only lets us inject the notice into `MDXContent`'s
 * children, which renders below a frontmatter (synthetic) title but above an
 * inline `# h1` title.
 *
 * Mirrors the upstream component:
 * @see {@link https://github.com/facebook/docusaurus/blob/main/packages/docusaurus-theme-classic/src/theme/DocItem/Content/index.tsx}
 */
export default function ContentWrapper({ children }: Props): ReactNode {
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
