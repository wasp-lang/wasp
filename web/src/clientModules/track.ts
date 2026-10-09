import ExecutionEnvironment from "@docusaurus/ExecutionEnvironment";

import { track, type AnalyticsEventName } from "../lib/analytics";
import {
  classifyCopiedText,
  isShellLanguage,
} from "../lib/analytics-classifier";

const CODE_BLOCK_SELECTOR = ".theme-code-block";
// Match on `title`: Docusaurus changes the `aria-label` to "Copied" after a copy.
const CODE_BLOCK_COPY_BUTTON_SELECTOR = `${CODE_BLOCK_SELECTOR} button[title="Copy"]`;

// Checked before `data-placement`: the mobile menu reuses the navbar item props.
const MOBILE_MENU_SELECTOR = ".navbar-sidebar, .theme-layout-navbar-sidebar";

const PLACEMENT_BY_THEME_SELECTOR: [selector: string, placement: string][] = [
  [".theme-announcement-bar", "announcement_bar"],
  [".theme-layout-navbar, .navbar", "header"],
  [".theme-doc-sidebar-container", "sidebar"],
  [".theme-doc-toc-desktop, .theme-doc-toc-mobile", "toc"],
  [".theme-doc-footer", "doc_footer"],
  [".theme-layout-footer, .footer", "footer"],
];

const DEFAULT_PLACEMENT = "body";

declare global {
  interface Window {
    __waspAnalyticsListening?: boolean;
  }
}

if (ExecutionEnvironment.canUseDOM && !window.__waspAnalyticsListening) {
  window.__waspAnalyticsListening = true;
  // Capture phase, because some components stop the propagation of their clicks.
  document.addEventListener("click", handleClick, { capture: true });
  document.addEventListener("copy", handleCopy, { capture: true });
}

function handleClick(event: MouseEvent): void {
  if (!(event.target instanceof Element)) {
    return;
  }

  const copyButton = event.target.closest(CODE_BLOCK_COPY_BUTTON_SELECTOR);
  if (copyButton) {
    const codeBlock = copyButton.closest(CODE_BLOCK_SELECTOR);
    if (codeBlock) {
      trackCodeCopy(getCodeBlockText(codeBlock), codeBlock, "button");
    }
    return;
  }

  const link = event.target.closest<HTMLAnchorElement>("a[href]");
  if (link) {
    trackLinkClick(link);
  }
}

function handleCopy(): void {
  // The Docusaurus copy fallback copies from a hidden textarea; the click already counted it.
  const activeElement = document.activeElement;
  if (
    activeElement instanceof HTMLTextAreaElement ||
    activeElement instanceof HTMLInputElement
  ) {
    return;
  }

  const selection = document.getSelection();
  const text = selection?.toString() ?? "";
  const anchorNode = selection?.anchorNode;
  const anchorElement =
    anchorNode instanceof Element ? anchorNode : anchorNode?.parentElement;
  if (!text.trim() || !anchorElement) {
    return;
  }

  const copyBlock = anchorElement.closest<HTMLElement>("[data-copy-event]");
  if (copyBlock) {
    trackCopyBlockSelection(copyBlock);
    return;
  }

  const codeBlock = anchorElement.closest(CODE_BLOCK_SELECTOR);
  trackCodeCopy(text, codeBlock ?? anchorElement, "selection");
}

function trackCopyBlockSelection(copyBlock: HTMLElement): void {
  const { copyEvent, copyKind } = copyBlock.dataset;
  track(copyEvent as AnalyticsEventName, {
    ...(copyKind ? { kind: copyKind } : {}),
    placement: resolvePlacement(copyBlock),
    method: "selection",
  });
}

function trackCodeCopy(
  text: string,
  element: Element,
  method: "button" | "selection",
): void {
  const codeBlock = element.closest(CODE_BLOCK_SELECTOR);
  const result = classifyCopiedText(text, {
    isShellBlock: codeBlock !== null && isShellLanguage(getLanguage(codeBlock)),
  });
  if (!result) {
    return;
  }

  track(result.name, {
    ...result.props,
    placement: resolvePlacement(element),
    method,
  });
}

function trackLinkClick(link: HTMLAnchorElement): void {
  const placement = resolvePlacement(link);

  if (
    link.closest("[data-track]")?.getAttribute("data-track") === "get_started"
  ) {
    track("Get Started: Click", { placement });
    return;
  }

  let url: URL;
  try {
    url = new URL(link.href, window.location.href);
  } catch {
    return;
  }

  const leavesPage =
    url.origin !== window.location.origin && link.target !== "_blank";

  if (isDiscordUrl(url)) {
    track("Discord: Click", { placement }, { leavesPage });
  } else if (isWaspGitHubUrl(url)) {
    track(
      "GitHub: Click",
      { placement, target: getGitHubTarget(url) },
      { leavesPage },
    );
  } else if (isOpenSaasUrl(url)) {
    track("Open SaaS: Click", { placement }, { leavesPage });
  }
}

function resolvePlacement(element: Element): string {
  if (element.closest(MOBILE_MENU_SELECTOR)) {
    return "mobile_menu";
  }

  const ownPlacement = element
    .closest("[data-placement]")
    ?.getAttribute("data-placement");
  if (ownPlacement) {
    return ownPlacement;
  }

  for (const [selector, placement] of PLACEMENT_BY_THEME_SELECTOR) {
    if (element.closest(selector)) {
      return placement;
    }
  }

  return DEFAULT_PLACEMENT;
}

function getCodeBlockText(codeBlock: Element): string {
  const lines = codeBlock.querySelectorAll(".token-line");
  if (lines.length > 0) {
    return Array.from(lines, (line) => line.textContent ?? "").join("\n");
  }
  return codeBlock.querySelector("code")?.textContent ?? "";
}

function getLanguage(codeBlock: Element): string | undefined {
  const languageClass = Array.from(codeBlock.classList).find((className) =>
    className.startsWith("language-"),
  );
  return languageClass?.slice("language-".length);
}

function isDiscordUrl(url: URL): boolean {
  const host = url.hostname.replace(/^www\./, "");
  return (
    host === "discord.gg" ||
    (host === "discord.com" && url.pathname.startsWith("/invite/")) ||
    (url.origin === window.location.origin &&
      url.pathname.startsWith("/discord/"))
  );
}

function isWaspGitHubUrl(url: URL): boolean {
  const host = url.hostname.replace(/^www\./, "");
  return (
    host === "github.com" &&
    url.pathname.toLowerCase().startsWith("/wasp-lang/")
  );
}

function isOpenSaasUrl(url: URL): boolean {
  const host = url.hostname.replace(/^www\./, "");
  return host === "opensaas.sh" || host === "docs.opensaas.sh";
}

function getGitHubTarget(url: URL): string {
  const [, , repo, section] = url.pathname.toLowerCase().split("/");
  if (repo !== "wasp") {
    return "other_repo";
  }
  if (section === "edit") {
    return "edit_page";
  }
  if (section === "issues") {
    return "issues";
  }
  if (url.pathname.toLowerCase().includes("/examples")) {
    return "examples";
  }
  return "repo";
}
