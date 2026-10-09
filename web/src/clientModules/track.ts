import ExecutionEnvironment from "@docusaurus/ExecutionEnvironment";

import { track, type GitHubTarget } from "../lib/analytics";
import {
  classifyCopiedText,
  isShellLanguage,
  type AgentPluginKind,
} from "../lib/analytics-classifier";

const CODE_BLOCK_SELECTOR = ".theme-code-block";
// Match on `title`: Docusaurus changes the `aria-label` to "Copied" after a copy.
const CODE_BLOCK_COPY_BUTTON_SELECTOR = `${CODE_BLOCK_SELECTOR} button[title="Copy"]`;

// A component with its own copy button sets `data-track-event` (and
// `data-track-kind`), so a copy of its text by hand sends the same event.
const COPY_BLOCK_SELECTOR = '[data-track-event$=": Copy"]';

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

  const copyBlock = anchorElement.closest<HTMLElement>(COPY_BLOCK_SELECTOR);
  if (copyBlock) {
    trackCopyBlockSelection(copyBlock);
    return;
  }

  const codeBlock = anchorElement.closest(CODE_BLOCK_SELECTOR);
  trackCodeCopy(text, codeBlock ?? anchorElement, "selection");
}

function trackCopyBlockSelection(copyBlock: HTMLElement): void {
  const { trackEvent, trackKind } = copyBlock.dataset;
  if (trackEvent === "Install Command: Copy") {
    track(copyBlock, trackEvent, { method: "selection" });
  } else if (trackEvent === "Agent Plugin: Copy" && trackKind) {
    track(copyBlock, trackEvent, {
      method: "selection",
      kind: trackKind as AgentPluginKind,
    });
  }
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

  switch (result?.name) {
    case "Install Command: Copy":
      track(element, result.name, { method });
      break;
    case "Agent Plugin: Copy":
      track(element, result.name, { method, kind: result.props.kind });
      break;
    case "CLI Command: Copy":
      track(element, result.name, { method, command: result.props.command });
      break;
  }
}

function trackLinkClick(link: HTMLAnchorElement): void {
  if (
    link.closest("[data-track-event]")?.getAttribute("data-track-event") ===
    "Get Started: Click"
  ) {
    track(link, "Get Started: Click", {});
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
    track(link, "Discord: Click", {}, { leavesPage });
  } else if (isWaspGitHubUrl(url)) {
    track(
      link,
      "GitHub: Click",
      { target: getGitHubTarget(url) },
      { leavesPage },
    );
  } else if (isOpenSaasUrl(url)) {
    track(link, "Open SaaS: Click", {}, { leavesPage });
  }
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

function getGitHubTarget(url: URL): GitHubTarget {
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
