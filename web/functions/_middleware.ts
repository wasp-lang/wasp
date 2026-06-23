/**
 * Cloudflare Pages Function: https://developers.cloudflare.com/pages/functions/middleware/
 *
 * Content negotiation for LLMs: when a client asks for Markdown via the `Accept`
 * header, serve the pre-generated `.md` sibling of the requested docs page instead
 * of the HTML.
 */

import { isValidMarkdownDocsRoute } from "../scripts/markdown-docs/html-to-md/markdown-routes";

interface CloudflarePagesContext {
  request: Request;
  next: (input?: Request | string, init?: RequestInit) => Promise<Response>;
}

export const onRequest = async (
  context: CloudflarePagesContext,
): Promise<Response> => {
  const { request, next } = context;

  const url = new URL(request.url);
  if (
    isAlreadyMarkdownRoute(url.pathname) ||
    !isValidMarkdownDocsRoute(url.pathname)
  ) {
    return next();
  }

  // Content negotiation starts here. Return with `Vary` header.
  if (!wantsMarkdownContent(request)) {
    return fallbackToHtmlResponse(next);
  }

  const markdownPathname = generateMarkdownPathname(url.pathname);
  const markdownUrl = new URL(markdownPathname, url.origin);
  const markdownResponse = await next(new Request(markdownUrl, request));

  if (!markdownResponse.ok) {
    return fallbackToHtmlResponse(next);
  }

  markdownResponse.headers.set("Content-Type", "text/markdown; charset=utf-8");
  markdownResponse.headers.set("Vary", "Accept");
  return markdownResponse;
};

function isAlreadyMarkdownRoute(pathname: string): boolean {
  return pathname.endsWith(".md");
}

function wantsMarkdownContent(request: Request): boolean {
  if (request.method !== "GET") {
    return false;
  }
  const acceptHeader = request.headers.get("Accept") ?? "";
  // We don't really want to bother with order of formats and their q-values.
  // Requesting `text/markdown` is a deliberate choice, so we assume priority.
  return acceptHeader.includes("text/markdown");
}

async function fallbackToHtmlResponse(next: CloudflarePagesContext["next"]) {
  const htmlResponse = await next();
  htmlResponse.headers.set("Vary", "Accept");
  return htmlResponse;
}

function generateMarkdownPathname(pathname: string): string {
  // This middleware runs before trailing slash stripping happens.
  return pathname.replace(/\/+$/, "") + ".md";
}
