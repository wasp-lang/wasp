import { isRouteWithMarkdownVariant } from "../scripts/markdown-docs/html-to-md/markdown-routes";

interface CloudflarePagesContext {
  request: Request;
  next: (input?: Request | string, init?: RequestInit) => Promise<Response>;
}

/**
 * Cloudflare middleware: https://developers.cloudflare.com/pages/functions/middleware/
 *
 * Handles markdown content negotiation for markdown docs.
 * When a client asks for Markdown via the `Accept` header,
 * serve the pre-generated `.md` sibling of the requested page
 * instead of the HTML.
 */
export const onRequest = async (
  context: CloudflarePagesContext,
): Promise<Response> => {
  const { request, next } = context;
  const url = new URL(request.url);

  const isMarkdownContentNegotiationRoute =
    isRouteWithMarkdownVariant(url.pathname) &&
    !isSpecificFileTypeRoute(url.pathname);
  if (!isMarkdownContentNegotiationRoute) {
    return next();
  }

  let contentNegotiationResponse: Response | undefined;
  if (acceptsMarkdownContent(request)) {
    contentNegotiationResponse = await fetchMarkdownVariant(context);
  } else {
    contentNegotiationResponse = await next();
  }
  // A response whose return content was influenced by a request, must include
  // the reason why in the `Vary` HTTP header. For us, that is the `Accept` header.
  // This ensures a cache will keep separate cached responses for different `Accept` values.
  contentNegotiationResponse.headers.set("Vary", "Accept");

  return contentNegotiationResponse;
};

/**
 * True if the last path segmenet includes a dot.
 *
 * @example "/docs.md"
 * @example "/docs.html"
 */
function isSpecificFileTypeRoute(pathname: string): boolean {
  return pathname.split("/").at(-1)!.includes(".");
}

function acceptsMarkdownContent(request: Request): boolean {
  if (request.method !== "GET") {
    return false;
  }
  const acceptHeader = request.headers.get("Accept") ?? "";
  // We don't really want to bother with format priorities (order of formats or q-values).
  // Requesting `text/markdown` is a deliberate choice, so we assume it as the top priority.
  return acceptHeader.includes("text/markdown");
}

async function fetchMarkdownVariant(
  context: CloudflarePagesContext,
): Promise<Response> {
  const { request, next } = context;
  const url = new URL(request.url);

  const markdownPathname = generateMarkdownPathname(url.pathname);
  const markdownUrl = new URL(markdownPathname, url.origin);
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
