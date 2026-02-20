import { useDoc } from "@docusaurus/plugin-content-docs/client";
import type { WrapperProps } from "@docusaurus/types";
import LastCheckedWithVersionsNotice, {
  LastCheckedWithVersions,
} from "@site/src/components/VersionNotice";
import Content from "@theme-original/DocItem/Content";
import type ContentType from "@theme/DocItem/Content";
import type { ReactNode } from "react";

type Props = WrapperProps<typeof ContentType>;

interface FrontMatter {
  last_checked_with_versions?: LastCheckedWithVersions;
}

export default function ContentWrapper(props: Props): ReactNode {
  const frontMatter = useDoc().frontMatter as FrontMatter;

  return (
    <>
      {frontMatter.last_checked_with_versions && (
        <div className="mt-4">
          <LastCheckedWithVersionsNotice
            lastCheckedWithVersions={frontMatter.last_checked_with_versions}
          />
        </div>
      )}

      <Content {...props} />
    </>
  );
}
