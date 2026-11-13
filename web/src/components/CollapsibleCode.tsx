import { SKIP_IN_MARKDOWN_DOCS_CLASS } from "@site/src/lib/constants";
import React, { useId, useRef, useState } from "react";
import styles from "./CollapsibleCode.module.css";

export default function CollapsibleCode({
  children,
}: {
  children: React.ReactNode;
}) {
  const [isExpanded, setIsExpanded] = useState(false);
  const contentId = useId();
  const rootRef = useRef<HTMLDivElement>(null);

  function handleToggle() {
    const root = rootRef.current;
    const isTopOutOfView = root && root.getBoundingClientRect().top < 0;
    if (isExpanded && isTopOutOfView) {
      root.scrollIntoView();
    }
    setIsExpanded(!isExpanded);
  }

  return (
    <div ref={rootRef} className={styles.root}>
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
        onClick={handleToggle}
      >
        {isExpanded ? "Collapse" : "Expand"}
      </button>
    </div>
  );
}
