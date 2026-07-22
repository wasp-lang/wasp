import type { Props } from "@theme/TOC";
import TOCItems from "@theme/TOCItems";
import clsx from "clsx";
import { useEffect, useRef, type ReactNode } from "react";

import styles from "./styles.module.css";

const LINK_CLASS_NAME = "table-of-contents__link toc-highlight";
const LINK_ACTIVE_CLASS_NAME = "table-of-contents__link--active";
const SCROLL_PADDING_PX = 1;

function scrollActiveLinkIntoView(container: HTMLDivElement): void {
  const activeLink = container.querySelector<HTMLElement>(
    `.${LINK_ACTIVE_CLASS_NAME}`,
  );

  if (!activeLink) {
    return;
  }

  const containerRect = container.getBoundingClientRect();
  const activeLinkRect = activeLink.getBoundingClientRect();

  if (activeLinkRect.top < containerRect.top) {
    container.scrollTop +=
      Math.floor(activeLinkRect.top - containerRect.top) - SCROLL_PADDING_PX;
  } else if (activeLinkRect.bottom > containerRect.bottom) {
    container.scrollTop +=
      Math.ceil(activeLinkRect.bottom - containerRect.bottom) +
      SCROLL_PADDING_PX;
  }
}

export default function TOC({ className, ...props }: Props): ReactNode {
  const containerRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    const container = containerRef.current;

    if (!container) {
      return undefined;
    }

    const observer = new MutationObserver(() => {
      scrollActiveLinkIntoView(container);
    });

    observer.observe(container, {
      attributes: true,
      attributeFilter: ["class"],
      childList: true,
      subtree: true,
    });

    scrollActiveLinkIntoView(container);

    return () => observer.disconnect();
  }, []);

  return (
    <div
      ref={containerRef}
      className={clsx(styles.tableOfContents, "thin-scrollbar", className)}
    >
      <TOCItems
        {...props}
        linkClassName={LINK_CLASS_NAME}
        linkActiveClassName={LINK_ACTIVE_CLASS_NAME}
      />
    </div>
  );
}
