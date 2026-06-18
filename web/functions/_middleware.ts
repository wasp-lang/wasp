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

  if (!wantsMarkdownContent(request)) {
    return next();
  }

  const url = new URL(request.url);
  if (isAlreadyMarkdownRoute(url.pathname)) {
    return next();
  }
  if (!isValidMarkdownDocsRoute(url.pathname)) {
    return next();
  }

  const markdownPathname = generateMarkdownPathname(url.pathname);
  if (!markdownPathname) {
    return next();
  }

  const markdownUrl = new URL(markdownPathname, url.origin);
  const markdownResponse = await next(new Request(markdownUrl, request));
  // TODO: Check headers are a okay.
  markdownResponse.headers.set("Vary", "Accept");
  markdownResponse.headers.set("Content-Type", "text/markdown; charset=utf-8");
  return markdownResponse;
};

function wantsMarkdownContent(request: Request): boolean {
  if (request.method !== "GET") {
    return false;
  }
  const acceptHeader = request.headers.get("Accept") ?? "";
  // We don't really want to borther with order of formats and their q-values.
  // Requesting `text/markdown` is a deliberate choice, so we assume priority.
  return acceptHeader.includes("text/markdown");
}

function isAlreadyMarkdownRoute(pathname: string): boolean {
  return pathname.endsWith(".md");
}

function generateMarkdownPathname(pathname: string): string {
  // This middleware runs before trialing slash stripping happens.
  return pathname.replace(/\/+$/, "") + ".md";
}
