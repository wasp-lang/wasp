import type { RedirectRule } from "./src/plugins/cloudflare-redirects";
import docsVersions from "./versions.json";

export function getRedirects({
  includeCurrentVersion,
}: {
  includeCurrentVersion: boolean;
}): RedirectRule[] {
  const redirects: RedirectRule[] = [...hardcodedRedirects];

  if (!includeCurrentVersion) {
    // Redirects explicitly versioned links for the current version to the
    // unprefixed canonical path, e.g. /docs/0.24/quick-start -> /docs/quick-start.
    const currentVersion = docsVersions[0];

    redirects.push({
      from: `/docs/${currentVersion}/*`,
      to: "/docs/:splat",
      code: 302,
    });
  }

  return redirects;
}

// Cloudflare Pages server redirects.
// Order matters: Cloudflare applies the first matching rule, so list more
// specific rules before more general ones.
// prettier-ignore
const hardcodedRedirects: RedirectRule[] = [
  { from: "/docs/advanced/deployment/overview",       to: "/docs/deployment/intro",                                   code: 301 },
  { from: "/docs/data-model/backends",                to: "/docs/data-model/databases",                               code: 301 },
  { from: "/docs/deploying",                          to: "/docs/deployment/intro",                                   code: 301 },
  { from: "/docs/deployment/deployment-methods/cli",  to: "/docs/deployment/deployment-methods/wasp-deploy/overview", code: 301 },
  { from: "/docs/deployment/deployment-methods/paas", to: "/docs/deployment/deployment-methods/cloud-providers",      code: 301 },
  { from: "/docs/general/language",                   to: "/docs/guides/legacy/wasp-dsl",                             code: 301 },
  { from: "/docs/general/wasp-ts-config",             to: "/docs/guides/legacy/wasp-ts-config",                       code: 301 },
  { from: "/docs/guides/auth-ui",                     to: "/docs/auth/ui",                                            code: 301 },
  { from: "/docs/guides/crud",                        to: "/docs/data-model/crud",                                    code: 301 },
  { from: "/docs/guides/email-auth",                  to: "/docs/auth/email",                                         code: 301 },
  { from: "/docs/guides/middleware-customization",    to: "/docs/advanced/middleware-config",                         code: 301 },
  { from: "/docs/guides/testing",                     to: "/docs/project/testing",                                    code: 301 },
  { from: "/docs/guides/username-password",           to: "/docs/auth/username-and-pass",                             code: 301 },
  { from: "/docs/guides/websockets",                  to: "/docs/advanced/web-sockets",                               code: 301 },
  { from: "/docs/integrations/css-frameworks",        to: "/docs/project/css-frameworks",                             code: 301 },
  { from: "/docs/integrations/github",                to: "/docs/auth/social-auth/github",                            code: 301 },
  { from: "/docs/integrations/google",                to: "/docs/auth/social-auth/google",                            code: 301 },
  { from: "/docs/project/css-frameworks",             to: "/docs/guides/libraries/tailwind",                          code: 301 },
  { from: "/docs/tutorials/todo-app",                 to: "/docs/tutorial/create",                                    code: 301 },

  // Migration guides
  // We migrated from /docs/{version}/migration-guides/migrate-from-{version}-to-{version}
  // to /docs/{version}/migration-guide, so we want to redirect links going to the old
  // URLs.
  // You don't need to add new redirects for new versions.
  { from: "/*/migrate-from-0-11-to-0-12", to: "/docs/0.12/migration-guide", code: 301 },
  { from: "/*/migrate-from-0-12-to-0-13", to: "/docs/0.13/migration-guide", code: 301 },
  { from: "/*/migrate-from-0-13-to-0-14", to: "/docs/0.14/migration-guide", code: 301 },
  { from: "/*/migrate-from-0-14-to-0-15", to: "/docs/0.15/migration-guide", code: 301 },
  { from: "/*/migrate-from-0-15-to-0-16", to: "/docs/0.16/migration-guide", code: 301 },
  { from: "/*/migrate-from-0-16-to-0-17", to: "/docs/0.17/migration-guide", code: 301 },
  { from: "/*/migrate-from-0-17-to-0-18", to: "/docs/0.18/migration-guide", code: 301 },
  { from: "/*/migrate-from-0-18-to-0-19", to: "/docs/0.19/migration-guide", code: 301 },
  { from: "/*/migrate-from-0-19-to-0-20", to: "/docs/0.20/migration-guide", code: 301 },
  { from: "/*/migrate-from-0-20-to-0-21", to: "/docs/0.21/migration-guide", code: 301 },
  { from: "/*/migrate-from-0-21-to-0-22", to: "/docs/0.22/migration-guide", code: 301 },
  { from: "/*/migrate-from-0-22-to-0-23", to: "/docs/0.23/migration-guide", code: 301 },

  // Legacy version path redirects (0.X.0 -> 0.X)
  // These exist because docs versions used to be named 0.X.0 but were renamed to 0.X,
  // so we want to redirect links going to the old URLs.
  // You don't need to add new redirects for new versions.
  { from: "/docs/0.12.0/*", to: "/docs/0.12/:splat", code: 301 },
  { from: "/docs/0.13.0/*", to: "/docs/0.13/:splat", code: 301 },
  { from: "/docs/0.14.0/*", to: "/docs/0.14/:splat", code: 301 },
  { from: "/docs/0.15.0/*", to: "/docs/0.15/:splat", code: 301 },
  { from: "/docs/0.16.0/*", to: "/docs/0.16/:splat", code: 301 },
  { from: "/docs/0.17.0/*", to: "/docs/0.17/:splat", code: 301 },
  { from: "/docs/0.18.0/*", to: "/docs/0.18/:splat", code: 301 },
  { from: "/docs/0.19.0/*", to: "/docs/0.19/:splat", code: 301 },
  { from: "/docs/0.20.0/*", to: "/docs/0.20/:splat", code: 301 },
];
