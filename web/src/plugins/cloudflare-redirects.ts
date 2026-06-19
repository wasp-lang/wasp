import type { PluginModule } from "@docusaurus/types";
import * as fs from "fs/promises";
import * as path from "path";

export interface PluginOptions {
  /**
   * Rules are order-dependent: Cloudflare applies the first matching rule, so
   * list more specific rules before more general ones.
   */
  redirects: RedirectRule[];
}

/**
 * Source and destination use Cloudflare Pages redirect syntax, including
 * `*` wildcards and `:splat`/`:placeholder` references.
 * See https://developers.cloudflare.com/pages/configuration/redirects/
 */
export interface RedirectRule {
  from: string;
  to: string;
  /** @default 302 */
  code?: number;
}

const DEFAULT_CODE = 302;

// Docusaurus copies `static/` files verbatim, but redirects are easier to
// maintain as typed config. This plugin writes the Cloudflare Pages `_redirects`
// file (https://developers.cloudflare.com/pages/configuration/redirects/) into
// the build output at build time, so the redirect list can live in code.
export default function cloudflareRedirectsPlugin(
  options: PluginOptions,
): PluginModule {
  return () => ({
    name: "cloudflare-redirects",
    async postBuild({ outDir }) {
      const content = serializeRedirects(options.redirects);
      await fs.writeFile(path.join(outDir, "_redirects"), content, "utf8");
    },
  });
}

function serializeRedirects(redirects: RedirectRule[]): string {
  return [
    "# Cloudflare Pages server redirects",
    "# Generated at build time by the cloudflare-redirects plugin.",
    "",
    ...redirects.map(
      ({ from, to, code }) => `${from} ${to} ${code ?? DEFAULT_CODE}`,
    ),
    "",
  ].join("\n");
}
