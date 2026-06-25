import { isValidMarkdownDocsRoute } from "../scripts/markdown-docs/html-to-md/markdown-routes";

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
    isValidMarkdownDocsRoute(url.pathname) ||
    !isSpecifcFileTypeRoute(url.pathname);
  if (!isMarkdownContentNegotiationRoute) {
    return next();
  }

  let contentNegotiationResponse: Response | undefined;
  if (!wantsMarkdownContent(request)) {
    contentNegotiationResponse = await next();
  } else {
    const markdownUrl = new URL(
      generateMarkdownPathname(url.pathname),
      url.origin,
    );
    contentNegotiationResponse = await next(new Request(markdownUrl, request));
    if (!contentNegotiationResponse.ok) {
      console.log(
        `Markdown response failed with ${contentNegotiationResponse.status}: ${await contentNegotiationResponse.text()}`,
      );
    }
  }
  // All responses, whose return content was influenced by a request,
  // must include the reason in the `Vary` HTTP header.
  contentNegotiationResponse.headers.set("Vary", "Accept");
  return contentNegotiationResponse;
};

/**
 * Ture if the last path segmenet includes a dot.
 *
 * @example "/docs.md"
 * @example "/docs.html"
 */
function isSpecifcFileTypeRoute(pathname: string): boolean {
  return pathname.split("/").at(-1)!.includes(".");
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

function generateMarkdownPathname(pathname: string): string {
  return pathname.replace(/\/+$/, "") + ".md";
}
