import { routeHasMarkdownVariant } from "../src/plugins/llm-files/markdown-docs/markdown-routes";

interface CloudflarePagesContext {
  request: Request;
  next: (input?: Request | string, init?: RequestInit) => Promise<Response>;
}

/**
 * Cloudflare middleware entry function which handles markdown content negotiation for docs.
 * When a client asks for a markdown variant of some docs via the `Accept` header,
 * serve the pre-generated `.md` sibling of the requested page instead of the HTML.
 * Only works for valid markdown variant routes ({@link routeHasMarkdownVariant}).
 *
 * Runs as a Cloudflare Pages Function, so it executes on the Cloudflare Workers runtime.
 * This is not a Node.js runtime, so we must manage external dependencies carefully.
 * @see {@link https://developers.cloudflare.com/pages/functions/middleware/ Cloudflare middleware}
 */
export const onRequest = async (
  context: CloudflarePagesContext,
): Promise<Response> => {
  const { request, next } = context;

  if (request.method !== "GET") {
    return next();
  }

  const url = new URL(request.url);
  const canNegotiateContentType =
    routeHasMarkdownVariant(url.pathname) &&
    !routeHasFileTypeExtension(url.pathname);
  if (!canNegotiateContentType) {
    return next();
  }

  const acceptHeader = request.headers.get("Accept");
  let contentNegotiationResponse = acceptsMarkdown(acceptHeader)
    ? await fetchMarkdownVariant(context)
    : await next();

  // The response varies based on the `Accept` header, so we set the
  // `Vary: Accept` header to ensure caches maintain separate entries
  // for different `Accept` values.
  contentNegotiationResponse.headers.set("Vary", "Accept");

  return contentNegotiationResponse;
};

/**
 * True if the last path segmenet includes a dot.
 *
 * @example "/docs.md"
 * @example "/docs.html"
 */
function routeHasFileTypeExtension(pathname: string): boolean {
  return pathname.split("/").at(-1)!.includes(".");
}

/**
 * We don't really want to bother with format priorities (order of formats or q-values).
 * Requesting `text/markdown` is a deliberate choice, so we assume it as the top priority.
 */
function acceptsMarkdown(acceptHeader: string | null): boolean {
  return !!acceptHeader && acceptHeader.includes("text/markdown");
}

async function fetchMarkdownVariant(
  context: CloudflarePagesContext,
): Promise<Response> {
  const { request, next } = context;
  const url = new URL(request.url);

  const markdownPathname = generateMarkdownPathname(url.pathname);
  const markdownUrl = new URL(markdownPathname, url);
  const markdownRequest = new Request(markdownUrl, request);
  const markdownResponse = await next(markdownRequest);

  if (!markdownResponse.ok) {
    console.error("Markdown response failed", {
      status: markdownResponse.status,
      statusText: markdownResponse.statusText,
      pathname: markdownPathname,
      body: await markdownResponse.clone().text(),
    });
  }

  return markdownResponse;
}

function generateMarkdownPathname(pathname: string): string {
  return pathname.replace(/\/+$/, "") + ".md";
}
