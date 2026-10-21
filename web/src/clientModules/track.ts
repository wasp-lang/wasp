import ExecutionEnvironment from "@docusaurus/ExecutionEnvironment";

import {
  track,
  trackDeclaredClick,
  trackDeclaredCopy,
  type GitHubTarget,
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
  // The Docusaurus copy fallback copies from a hidden textarea; the copy
  // button already counted it.
  const activeElement = document.activeElement;
  if (
    activeElement instanceof HTMLTextAreaElement ||
    activeElement instanceof HTMLInputElement
  ) {
    return;
  }

  const selection = document.getSelection();
  const anchorNode = selection?.anchorNode;
  const anchorElement =
    anchorNode instanceof Element ? anchorNode : anchorNode?.parentElement;
  if (!selection?.toString().trim() || !anchorElement) {
    return;
  }

  trackDeclaredCopy(anchorElement, "selection");
}

// These links are in hundreds of Markdown files, so the URL decides the event.
function trackLinkClick(link: HTMLAnchorElement): void {
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
