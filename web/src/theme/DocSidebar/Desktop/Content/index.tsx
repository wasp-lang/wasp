import { ThemeClassNames } from "@docusaurus/theme-common";
import {
  useAnnouncementBar,
  useScrollPosition,
} from "@docusaurus/theme-common/internal";
import { translate } from "@docusaurus/Translate";
import type { Props } from "@theme/DocSidebar/Desktop/Content";
import DocSidebarItems from "@theme/DocSidebarItems";
import clsx from "clsx";
import { useEffect, useRef, useState, type ReactNode } from "react";
import styles from "./styles.module.css";

const ACTIVE_LINK_SELECTOR = "a.menu__link--active";
const SCROLL_PADDING_PX = 8;

function useShowAnnouncementBar() {
  const { isActive } = useAnnouncementBar();
  const [showAnnouncementBar, setShowAnnouncementBar] = useState(isActive);

  useScrollPosition(
    ({ scrollY }) => {
      if (isActive) {
        setShowAnnouncementBar(scrollY === 0);
      }
    },
    [isActive],
  );

  return isActive && showAnnouncementBar;
}

export default function DocSidebarDesktopContent({
  path,
  sidebar,
  className,
}: Props): ReactNode {
  const showAnnouncementBar = useShowAnnouncementBar();
  const containerRef = useRef<HTMLElement>(null);

  useEffect(() => {
    const container = containerRef.current;

    if (!container) {
      return undefined;
    }

    const animationFrameId = window.requestAnimationFrame(() => {
      scrollActiveLinkIntoView(container);
    });

    return () => {
      window.cancelAnimationFrame(animationFrameId);
    };
  }, [path]);

  return (
    <nav
      ref={containerRef}
      aria-label={translate({
        id: "theme.docs.sidebar.navAriaLabel",
        message: "Docs sidebar",
        description: "The ARIA label for the sidebar navigation",
      })}
      className={clsx(
        "menu thin-scrollbar",
        styles.menu,
        showAnnouncementBar && styles.menuWithAnnouncementBar,
        className,
      )}
    >
      <ul className={clsx(ThemeClassNames.docs.docSidebarMenu, "menu__list")}>
        <DocSidebarItems items={sidebar} activePath={path} level={1} />
      </ul>
    </nav>
  );
}

function scrollActiveLinkIntoView(container: HTMLElement): void {
  const activeLink = container.querySelector<HTMLElement>(ACTIVE_LINK_SELECTOR);

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
