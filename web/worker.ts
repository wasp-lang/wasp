import { getRedirects } from "./redirects";
import { routeHasMarkdownVariant } from "./src/plugins/llm-files/markdown-docs/markdown-routes";

interface Env {
  ASSETS: { fetch: (request: Request) => Promise<Response> };
}

/**
 * Cloudflare Worker entry function which handles markdown content negotiation for docs.
 * When a client asks for a markdown variant of some docs via the `Accept` header,
 * serve the pre-generated `.md` sibling of the requested page instead of the HTML.
 * Only works for valid markdown variant routes ({@link routeHasMarkdownVariant}).
 * Only handles routes listed in `assets.run_worker_first` in `wrangler.jsonc`,
 * all other requests are served directly from static assets.
 *
 * Runs on the Cloudflare Workers runtime.
 * This is not a Node.js runtime, so we must manage external dependencies carefully.
 * @see {@link https://developers.cloudflare.com/workers/static-assets/binding/ Cloudflare static assets}
 */
export default {
  async fetch(request: Request, env: Env): Promise<Response> {
    if (!["GET", "HEAD"].includes(request.method)) {
      return env.ASSETS.fetch(request);
    }

    const url = new URL(request.url);

    // We let `_redirects` handle the routes with a redirect rule.
    if (routeHasRedirectRule(url.pathname)) {
      return env.ASSETS.fetch(request);
    }

    const canNegotiateContentType =
      routeHasMarkdownVariant(url.pathname) &&
      !routeHasFileTypeExtension(url.pathname);
    if (!canNegotiateContentType) {
      return env.ASSETS.fetch(request);
    }

    const acceptHeader = request.headers.get("Accept");
    const assetResponse = acceptsMarkdown(acceptHeader)
      ? await fetchMarkdownVariant(request, env)
      : await env.ASSETS.fetch(request);

    // Asset responses have immutable headers, so we copy the response first.
    const contentNegotiationResponse = new Response(
      assetResponse.body,
      assetResponse,
    );

    // The response varies based on the `Accept` header, so we add `Accept`
    // to the `Vary` header to ensure caches maintain separate entries
    // for different `Accept` values.
    contentNegotiationResponse.headers.append("Vary", "Accept");

    return contentNegotiationResponse;
  },
};

/**
 * Redirects used to generate the `_redirects` file.
 */
const REDIRECT_FROM_PATTERNS = getRedirects({
  redirectCurrentVersionToCanonical: true,
}).map(({ from }) => redirectSourceToRegExp(from));

function routeHasRedirectRule(pathname: string): boolean {
  return REDIRECT_FROM_PATTERNS.some((pattern) => pattern.test(pathname));
}

function redirectSourceToRegExp(source: string): RegExp {
  const sourcePattern = source.split("*").map(escapeRegExp).join("(.*)");
  return new RegExp(`^${sourcePattern}$`);
}

function escapeRegExp(value: string): string {
  return value.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}

/**
 * True if the route targets a MD or HTML type directly.
 *
 * @example "/docs.md" → true
 * @example "/docs.html" → true
 * @example "/docs/0.24" → false
 * @example "/docs/api/@wasp.sh" → false
 */
function routeHasFileTypeExtension(pathname: string): boolean {
  return /\.(md|html)$/i.test(pathname);
}

/**
 * True if the request accepts markdown content.
 *
 * We don't really want to bother with format priorities (order of formats or q-values).
 * Requesting `text/markdown` is a deliberate choice, so we assume it as the top priority.
 */
function acceptsMarkdown(acceptHeader: string | null): boolean {
  return !!acceptHeader && acceptHeader.includes("text/markdown");
}

/**
 * Fetches the pre-generated markdown variant of the requested page.
 * A route missing its markdown variant intentionally results in a
 * 404 response.
 */
async function fetchMarkdownVariant(
  request: Request,
  env: Env,
): Promise<Response> {
  const url = new URL(request.url);

  const markdownPathname = generateMarkdownPathname(url.pathname);
  const markdownUrl = new URL(markdownPathname, url);
  const markdownRequest = new Request(markdownUrl, request);
  return env.ASSETS.fetch(markdownRequest);
}

/**
 * Maps an extensionless route pathname to the pathname of its
 * pre-generated markdown variant file.
 *
 * @example "/docs/quick-start" → "/docs/quick-start.md"
 * @example "/docs/" → "/docs.md"
 */
function generateMarkdownPathname(pathname: string): string {
  return pathname.replace(/\/$/, "") + ".md";
}
