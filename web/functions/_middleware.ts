/**
 * Cloudflare Pages Function: https://developers.cloudflare.com/pages/functions/middleware/
 *
 * Content negotiation for LLMs: when a client asks for Markdown via the `Accept`
 * header, serve the pre-generated `.md` sibling of the requested docs page instead
 * of the HTML.
 */

import { isMarkdownRoute } from "../scripts/markdown-docs/html-to-md/markdown-routes";

interface CloudflarePagesContext {
  request: Request;
  next: (input?: Request | string, init?: RequestInit) => Promise<Response>;
}

export const onRequest = async (
  context: CloudflarePagesContext,
): Promise<Response> => {
  const { request, next } = context;

  if (!wantsMarkdown(request)) {
    return next();
  }

  const url = new URL(request.url);
  if (isAlreadyRequestingMarkdown(url)) {
    return next();
  }
  if (!isMarkdownRoute(url.pathname)) {
    return next();
  }

  const markdownPath = generateMarkdownPath(url.pathname);
  if (!markdownPath) {
    return next();
  }

  const markdownUrl = new URL(markdownPath, url.origin);
  const markdownResponse = await next(new Request(markdownUrl, request));
  if (!markdownResponse.ok) {
    return next();
  }

  // TODO: check if we will have change the response status / headers.
  return markdownResponse;
};

function wantsMarkdown(request: Request): boolean {
  if (request.method !== "GET") {
    return false;
  }
  const acceptHeader = request.headers.get("Accept") ?? "";
  // We don't really want to borther with order of formats and their q-values.
  // Requesting `text/markdown` is a deliberate choice, so we assume priority.
  return acceptHeader.includes("text/markdown");
}

function isAlreadyRequestingMarkdown(url: URL): boolean {
  return url.pathname.endsWith(".md");
}

function generateMarkdownPath(pathname: string): string {
  // This middleware runs before trialing slash stripping happens.
  return pathname.replace(/\/+$/, "") + ".md";
}
