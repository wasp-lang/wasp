import { SKIP_IN_MARKDOWN_DOCS_CLASS } from "@site/src/lib/constants";
import React, { useId, useState } from "react";
import styles from "./CollapsibleCode.module.css";

export default function CollapsibleCode({
  children,
}: {
  children: React.ReactNode;
}) {
  const [isExpanded, setIsExpanded] = useState(false);
  const contentId = useId();

  return (
    <div className={styles.root}>
      <div
        id={contentId}
        className={isExpanded ? styles.expanded : styles.collapsed}
      >
        {children}
      </div>
      <button
        type="button"
        className={`${styles.toggle} ${SKIP_IN_MARKDOWN_DOCS_CLASS}`}
        aria-expanded={isExpanded}
        aria-controls={contentId}
        onClick={() => setIsExpanded(!isExpanded)}
      >
        {isExpanded ? "Collapse" : "Expand"}
      </button>
    </div>
  );
}
