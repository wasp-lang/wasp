import { useState } from "react";

interface CopySectionButtonProps {
  /** The heading id (anchor) of the section to copy, e.g. "part-2-how-to-build-it". */
  sectionId: string;
  /** The start of that heading's text, used to find it in the post's Markdown. */
  headingPrefix: string;
  /** A sentence shown before the button. It's left out of the copied text. */
  intro?: string;
  label?: string;
}

type CopyState = "idle" | "copied" | "failed";

/**
 * Copies one `##` section of the post to the clipboard, so readers can hand it
 * to their coding agent. Uses the post's generated Markdown when the site
 * serves it (`<post-url>.md`), and falls back to the section's text on the
 * page otherwise (e.g. in `docusaurus start`).
 */
export function CopySectionButton({
  sectionId,
  headingPrefix,
  intro,
  label = "Copy this guide for your agent",
}: CopySectionButtonProps) {
  const [state, setState] = useState<CopyState>("idle");

  const copy = async () => {
    try {
      const section =
        (await sectionFromMarkdown(headingPrefix, label)) ??
        sectionFromPage(sectionId);
      if (!section) throw new Error(`Section "${sectionId}" not found`);
      const pageUrl = window.location.href.split("#")[0];
      await navigator.clipboard.writeText(
        `Source: ${pageUrl}#${sectionId}\n\n${section}`,
      );
      setState("copied");
    } catch {
      setState("failed");
    }
    setTimeout(() => setState("idle"), 2500);
  };

  return (
    <p data-copy-section-button>
      {intro && <>{intro} </>}
      <button
        type="button"
        className="button button--primary button--sm"
        onClick={copy}
      >
        {state === "copied"
          ? "Copied!"
          : state === "failed"
            ? "Couldn't copy, try again"
            : label}
      </button>
    </p>
  );
}

async function sectionFromMarkdown(
  headingPrefix: string,
  label: string,
): Promise<string | null> {
  const markdownUrl = window.location.pathname.replace(/\/$/, "") + ".md";
  const response = await fetch(markdownUrl).catch(() => null);
  if (!response?.ok) return null;
  const markdown = await response.text();
  if (!markdown.trimStart().startsWith(">") && !markdown.includes("\n# "))
    return null; // An HTML fallback page, not the generated Markdown.

  const lines = markdown.split("\n");
  const start = lines.findIndex((line) =>
    line.startsWith(`## ${headingPrefix}`),
  );
  if (start === -1) return null;

  // Stop at the next `## ` heading, ignoring ones inside code blocks.
  let inCodeBlock = false;
  let end = lines.length;
  for (let i = start + 1; i < lines.length; i++) {
    if (lines[i].trimStart().startsWith("```")) inCodeBlock = !inCodeBlock;
    if (!inCodeBlock && lines[i].startsWith("## ")) {
      end = i;
      break;
    }
  }
  return lines
    .slice(start, end)
    .filter((line) => !line.includes(label)) // This component's own paragraph.
    .join("\n")
    .replace(/\n{3,}/g, "\n\n")
    .trim();
}

function sectionFromPage(sectionId: string): string | null {
  const heading = document.getElementById(sectionId);
  if (!heading) return null;
  // Headings end with a zero-width space before Docusaurus's "#" link.
  const parts = [heading.innerText.replace(/[\u200B#]+$/, "").trim()];
  for (
    let el = heading.nextElementSibling;
    el && el.tagName !== "H2";
    el = el.nextElementSibling
  ) {
    if (el.hasAttribute("data-copy-section-button")) continue;
    if (el instanceof HTMLElement && el.innerText.trim())
      parts.push(el.innerText.trim());
  }
  return parts.join("\n\n");
}
