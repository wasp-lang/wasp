/**
 * Cloudflare Pages Function: https://developers.cloudflare.com/pages/functions/middleware/
 *
 * Content negotiation for LLMs: when a client asks for Markdown via the `Accept`
 * header, serve the pre-generated `.md` sibling of the requested docs page instead
 * of the HTML.
 */

interface CloudflarePagesContext {
  request: Request;
  next: (input?: Request | string, init?: RequestInit) => Promise<Response>;
}

// Routes that have generated `.md` counterparts.
const MARKDOWN_ROUTE_PREFIXES = ["/docs", "/blog", "/resources"];

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

/**
 * We server markdown assets only on {@link MARKDOWN_ROUTE_PREFIXES}.
 */
function isMarkdownRoute(pathname: string): boolean {
  return MARKDOWN_ROUTE_PREFIXES.some(
    (prefix) => pathname === prefix || pathname.startsWith(prefix + "/"),
  );
}

function generateMarkdownPath(pathname: string): string {
  // This middleware runs before trialing slash stripping happens.
  return pathname.replace(/\/+$/, "") + ".md";
}
