import ExecutionEnvironment from "@docusaurus/ExecutionEnvironment";

import {
  track,
  trackDeclaredClick,
  trackDeclaredCopy,
  type GitHubTarget,
  type TrackOptions,
} from "../lib/analytics";

if (ExecutionEnvironment.canUseDOM) {
  // Capture phase, because some components stop the propagation of their clicks.
  document.addEventListener("click", handleClick, { capture: true });
  document.addEventListener("copy", handleCopy, { capture: true });
}

function handleClick(event: MouseEvent): void {
  if (!(event.target instanceof Element)) {
    return;
  }

  trackDeclaredClick(event.target);

  const link = event.target.closest<HTMLAnchorElement>("a[href]");
  if (link) {
    trackLinkClick(link);
  }
}

function handleCopy(): void {
  if (isCopyFromFormField()) {
    return;
  }

  const selection = document.getSelection();
  const selectionStart = getSelectionStartElement(selection);
  if (!selection?.toString().trim() || !selectionStart) {
    return;
  }

  trackDeclaredCopy(selectionStart, "selection");
}

// These links are in hundreds of Markdown files, so the URL decides the event.
function trackLinkClick(link: HTMLAnchorElement): void {
  const url = parseLinkUrl(link);
  if (!url) {
    return;
  }

  const options: TrackOptions = { leavesPage: leavesThisPage(url, link) };
  if (isDiscordUrl(url)) {
    track(link, "Discord: Click", {}, options);
  } else if (isWaspGitHubUrl(url)) {
    track(link, "GitHub: Click", { target: getGitHubTarget(url) }, options);
  } else if (isOpenSaasUrl(url)) {
    track(link, "Open SaaS: Click", {}, options);
  }
}

// The Docusaurus copy fallback copies from a hidden textarea; the copy button
// already counted it.
function isCopyFromFormField(): boolean {
  const activeElement = document.activeElement;
  return (
    activeElement instanceof HTMLTextAreaElement ||
    activeElement instanceof HTMLInputElement
  );
}

// A selection often begins in a text node, so the element is its parent.
function getSelectionStartElement(selection: Selection | null): Element | null {
  const anchorNode = selection?.anchorNode;
  return anchorNode instanceof Element
    ? anchorNode
    : (anchorNode?.parentElement ?? null);
}

function parseLinkUrl(link: HTMLAnchorElement): URL | null {
  try {
    return new URL(link.href, window.location.href);
  } catch {
    return null;
  }
}

// A link to another site in the same tab unloads the page, so its event must go
// out with `sendBeacon`.
function leavesThisPage(url: URL, link: HTMLAnchorElement): boolean {
  return url.origin !== window.location.origin && link.target !== "_blank";
}

function isDiscordUrl(url: URL): boolean {
  const host = hostWithoutWww(url);
  return (
    host === "discord.gg" ||
    (host === "discord.com" && url.pathname.startsWith("/invite/")) ||
    (url.origin === window.location.origin &&
      url.pathname.startsWith("/discord/"))
  );
}

function isWaspGitHubUrl(url: URL): boolean {
  return (
    hostWithoutWww(url) === "github.com" &&
    url.pathname.toLowerCase().startsWith("/wasp-lang/")
  );
}

function isOpenSaasUrl(url: URL): boolean {
  const host = hostWithoutWww(url);
  return host === "opensaas.sh" || host === "docs.opensaas.sh";
}

function getGitHubTarget(url: URL): GitHubTarget {
  const path = url.pathname.toLowerCase();
  const [, , repo, section] = path.split("/");
  if (repo !== "wasp") {
    return "other_repo";
  }
  if (section === "edit") {
    return "edit_page";
  }
  if (section === "issues") {
    return "issues";
  }
  if (path.includes("/examples")) {
    return "examples";
  }
  return "repo";
}

function hostWithoutWww(url: URL): string {
  return url.hostname.replace(/^www\./, "");
}
