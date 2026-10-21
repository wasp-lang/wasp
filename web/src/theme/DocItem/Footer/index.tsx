import { useDoc } from "@docusaurus/plugin-content-docs/client";
import type { WrapperProps } from "@docusaurus/types";
import DocComments from "@site/src/components/DocComments";
import { trackPlacementAttributes } from "@site/src/lib/analytics";
import Footer from "@theme-original/DocItem/Footer";
import type FooterType from "@theme/DocItem/Footer";
import { type ReactNode } from "react";
import styles from "./styles.module.css";

type Props = WrapperProps<typeof FooterType>;

interface FrontMatter {
  comments?: boolean;
}

export default function FooterWrapper(props: Props): ReactNode {
  const frontMatter = useDoc().frontMatter as FrontMatter;

  return (
    <>
      <div {...trackPlacementAttributes("doc_footer")} className="contents">
        <Footer {...props} />
      </div>

      {frontMatter.comments && (
        <div className={styles.commentsWrapper}>
          <DocComments />
        </div>
      )}
    </>
  );
}
