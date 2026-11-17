import { docsReorganizationRedirectPaths } from "./docs-reorganization-redirects";
import imageRedirects from "./image-redirects.json";
import type { RedirectRule } from "./src/plugins/cloudflare-redirects";
import docsVersions from "./versions.json";

export function getRedirects({
  redirectCurrentVersionToCanonical,
}: {
  /**
   * Redirects explicitly versioned links for the current version to the
   * unprefixed canonical path, e.g.:
   * /docs/0.24/quick-start -> /docs/quick-start
   */
  redirectCurrentVersionToCanonical: boolean;
}): RedirectRule[] {
  // Order matters: Cloudflare applies the first matching rule, so list more
  // specific rules before more general ones. Cloudflare also recommends listing
  // static rules before rules with splats or placeholders, for performance.
  // TODO: Also redirect the `.md` versions of these pages.
  // https://github.com/wasp-lang/wasp/issues/4842
  const redirects: RedirectRule[] = [
    ...legacyDocsRedirects,
    ...docsReorganizationRedirects,
    ...imageReorganizationRedirects,
    ...legacyWildcardRedirects,
  ];

  if (redirectCurrentVersionToCanonical) {
    const latestWaspVersion = docsVersions[0];
    const redirect = temporary(`/docs/${latestWaspVersion}/*`, "/docs/:splat");
    redirects.push(redirect);
  }

  return redirects;
}

// prettier-ignore
const legacyDocsRedirects: RedirectRule[] = [
  permanent("/docs/advanced/deployment/overview",       "/docs/deployment/overview"),
  permanent("/docs/data-model/backends",                "/docs/features/data/databases"),
  permanent("/docs/deploying",                          "/docs/deployment/overview"),
  permanent("/docs/deployment/deployment-methods/cli",  "/docs/deployment/methods/wasp-deploy/overview"),
  permanent("/docs/deployment/deployment-methods/paas", "/docs/deployment/methods/cloud-providers"),
  permanent("/docs/editor-setup",                       "/docs/features/spec"),
  permanent("/docs/general/language",                   "/docs/guides/legacy/wasp-dsl"),
  permanent("/docs/general/typescript",                 "/docs/features/spec"),
  permanent("/docs/general/wasp-ts-config",             "/docs/guides/legacy/wasp-ts-config"),
  permanent("/docs/guides/auth-ui",                     "/docs/features/auth/ui"),
  permanent("/docs/guides/crud",                        "/docs/features/data/crud"),
  permanent("/docs/guides/email-auth",                  "/docs/features/auth/email/overview"),
  permanent("/docs/guides/middleware-customization",    "/docs/advanced/server-customization/middleware"),
  permanent("/docs/guides/testing",                     "/docs/advanced/testing"),
  permanent("/docs/guides/username-password",           "/docs/features/auth/username-and-pass/overview"),
  permanent("/docs/guides/websockets",                  "/docs/features/websockets"),
  permanent("/docs/integrations/css-frameworks",        "/docs/project/css-frameworks"),
  permanent("/docs/integrations/github",                "/docs/features/auth/social-auth/github"),
  permanent("/docs/integrations/google",                "/docs/features/auth/social-auth/google"),
  permanent("/docs/project/css-frameworks",             "/docs/guides/libraries/tailwind"),
  permanent("/docs/tutorials/todo-app",                 "/docs/tutorial/create"),
];

const docsReorganizationRedirects: RedirectRule[] =
  docsReorganizationRedirectPaths.map(({ from, to }) => permanent(from, to));

// Redirects for the reorganization of `static/img` into `blog/`, `docs/`,
// `resources/` and `site/` folders. They keep external links to the old image
// URLs working (social cards, newsletters, other sites).
// TODO: Consider removing these once traffic to the old URLs dies down.
const imageReorganizationRedirects: RedirectRule[] = imageRedirects.map(
  ({ from, to }) => permanent(from, to),
);

// prettier-ignore
const legacyWildcardRedirects: RedirectRule[] = [
  // Migration guides
  // We migrated from /docs/{version}/migration-guides/migrate-from-{version}-to-{version}
  // to /docs/{version}/migration-guide, so we want to redirect links going to the old
  // URLs.
  // You don't need to add new redirects for new versions.
  permanent("/*/migrate-from-0-11-to-0-12", "/docs/0.12/migration-guide"),
  permanent("/*/migrate-from-0-12-to-0-13", "/docs/0.13/migration-guide"),
  permanent("/*/migrate-from-0-13-to-0-14", "/docs/0.14/migration-guide"),
  permanent("/*/migrate-from-0-14-to-0-15", "/docs/0.15/migration-guide"),
  permanent("/*/migrate-from-0-15-to-0-16", "/docs/0.16/migration-guide"),
  permanent("/*/migrate-from-0-16-to-0-17", "/docs/0.17/migration-guide"),
  permanent("/*/migrate-from-0-17-to-0-18", "/docs/0.18/migration-guide"),
  permanent("/*/migrate-from-0-18-to-0-19", "/docs/0.19/migration-guide"),
  permanent("/*/migrate-from-0-19-to-0-20", "/docs/0.20/migration-guide"),
  permanent("/*/migrate-from-0-20-to-0-21", "/docs/0.21/migration-guide"),
  permanent("/*/migrate-from-0-21-to-0-22", "/docs/0.22/migration-guide"),
  permanent("/*/migrate-from-0-22-to-0-23", "/docs/0.23/migration-guide"),

  // Legacy version path redirects (0.X.0 -> 0.X)
  // These exist because docs versions used to be named 0.X.0 but were renamed to 0.X,
  // so we want to redirect links going to the old URLs.
  // You don't need to add new redirects for new versions.
  permanent("/docs/0.12.0/*", "/docs/0.12/:splat"),
  permanent("/docs/0.13.0/*", "/docs/0.13/:splat"),
  permanent("/docs/0.14.0/*", "/docs/0.14/:splat"),
  permanent("/docs/0.15.0/*", "/docs/0.15/:splat"),
  permanent("/docs/0.16.0/*", "/docs/0.16/:splat"),
  permanent("/docs/0.17.0/*", "/docs/0.17/:splat"),
  permanent("/docs/0.18.0/*", "/docs/0.18/:splat"),
  permanent("/docs/0.19.0/*", "/docs/0.19/:splat"),
  permanent("/docs/0.20.0/*", "/docs/0.20/:splat"),
];

/** Builds a permanent redirect rule (301). */
function permanent(from: string, to: string): RedirectRule {
  return { from, to, code: 301 };
}

/** Builds a temporary redirect rule (302). */
function temporary(from: string, to: string): RedirectRule {
  return { from, to, code: 302 };
}
