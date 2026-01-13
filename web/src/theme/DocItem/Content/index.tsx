import { useDoc } from "@docusaurus/plugin-content-docs/client";
import type { WrapperProps } from "@docusaurus/types";
import VersionNotice, {
  CheckedVersions,
} from "@site/src/components/VersionNotice";
import Content from "@theme-original/DocItem/Content";
import type ContentType from "@theme/DocItem/Content";
import type { ReactNode } from "react";
import styles from "./styles.module.css";

type Props = WrapperProps<typeof ContentType>;

interface FrontMatter {
  checked_versions?: CheckedVersions;
}

export default function ContentWrapper(props: Props): ReactNode {
  const frontMatter = useDoc().frontMatter as FrontMatter;

  return (
    <>
      {frontMatter.checked_versions && (
        <div className={styles.noticeWrapper}>
          <VersionNotice checkedVersions={frontMatter.checked_versions} />
        </div>
      )}

      <Content {...props} />
    </>
  );
}
