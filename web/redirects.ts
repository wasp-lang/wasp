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
  // specific rules before more general ones.
  const redirects: RedirectRule[] = [
    ...legacyDocsRedirects,
    ...docsReorganizationRedirects,
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
  permanent("/docs/guides/middleware-customization",    "/docs/advanced/server-customization/middleware-config"),
  permanent("/docs/guides/testing",                     "/docs/advanced/testing"),
  permanent("/docs/guides/username-password",           "/docs/features/auth/username-and-pass/overview"),
  permanent("/docs/guides/websockets",                  "/docs/features/web-sockets"),
  permanent("/docs/integrations/css-frameworks",        "/docs/project/css-frameworks"),
  permanent("/docs/integrations/github",                "/docs/features/auth/social-auth/github"),
  permanent("/docs/integrations/google",                "/docs/features/auth/social-auth/google"),
  permanent("/docs/project/css-frameworks",             "/docs/guides/libraries/tailwind"),
  permanent("/docs/tutorials/todo-app",                 "/docs/tutorial/create"),

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

// Redirects for the big docs reorganization that introduced the "Getting
// started", "Features" and "Advanced" sections.
// prettier-ignore
const docsReorganizationRedirects: RedirectRule[] = [
  permanent("/docs/advanced/apis",                                      "/docs/features/apis"),
  permanent("/docs/advanced/email",                                     "/docs/features/email"),
  permanent("/docs/advanced/jobs",                                      "/docs/features/jobs"),
  permanent("/docs/advanced/middleware-config",                         "/docs/advanced/server-customization/middleware-config"),
  permanent("/docs/advanced/routing",                                   "/docs/features/routing"),
  permanent("/docs/advanced/web-sockets",                               "/docs/features/web-sockets"),
  permanent("/docs/auth/advanced/custom-auth-actions",                  "/docs/features/auth/advanced/custom-auth-actions"),
  permanent("/docs/auth/auth-hooks",                                    "/docs/features/auth/hooks"),
  permanent("/docs/auth/email",                                         "/docs/features/auth/email/overview"),
  permanent("/docs/auth/email/create-your-own-ui",                      "/docs/features/auth/email/create-your-own-ui"),
  permanent("/docs/auth/entities/entities",                             "/docs/features/auth/entities"),
  permanent("/docs/auth/overview",                                      "/docs/features/auth/overview"),
  permanent("/docs/auth/social-auth/create-your-own-ui",                "/docs/features/auth/social-auth/create-your-own-ui"),
  permanent("/docs/auth/social-auth/discord",                           "/docs/features/auth/social-auth/discord"),
  permanent("/docs/auth/social-auth/github",                            "/docs/features/auth/social-auth/github"),
  permanent("/docs/auth/social-auth/google",                            "/docs/features/auth/social-auth/google"),
  permanent("/docs/auth/social-auth/keycloak",                          "/docs/features/auth/social-auth/keycloak"),
  permanent("/docs/auth/social-auth/microsoft",                         "/docs/features/auth/social-auth/microsoft"),
  permanent("/docs/auth/social-auth/overview",                          "/docs/features/auth/social-auth/overview"),
  permanent("/docs/auth/social-auth/slack",                             "/docs/features/auth/social-auth/slack"),
  permanent("/docs/auth/ui",                                            "/docs/features/auth/ui"),
  permanent("/docs/auth/username-and-pass",                             "/docs/features/auth/username-and-pass/overview"),
  permanent("/docs/auth/username-and-pass/create-your-own-ui",          "/docs/features/auth/username-and-pass/create-your-own-ui"),
  permanent("/docs/data-model/crud",                                    "/docs/features/data/crud"),
  permanent("/docs/data-model/databases",                               "/docs/features/data/databases"),
  permanent("/docs/data-model/entities",                                "/docs/features/data/entities"),
  permanent("/docs/data-model/operations/actions",                      "/docs/features/data/operations/actions"),
  permanent("/docs/data-model/operations/overview",                     "/docs/features/data/operations/overview"),
  permanent("/docs/data-model/operations/queries",                      "/docs/features/data/operations/queries"),
  permanent("/docs/data-model/prisma-file",                             "/docs/features/data/prisma-file"),
  permanent("/docs/deployment/deployment-methods/cloud-providers",      "/docs/deployment/methods/cloud-providers"),
  permanent("/docs/deployment/deployment-methods/overview",             "/docs/deployment/methods/overview"),
  permanent("/docs/deployment/deployment-methods/self-hosted",          "/docs/deployment/methods/self-hosted"),
  permanent("/docs/deployment/deployment-methods/wasp-deploy/ci-cd",    "/docs/deployment/methods/wasp-deploy/ci-cd"),
  permanent("/docs/deployment/deployment-methods/wasp-deploy/fly",      "/docs/deployment/methods/wasp-deploy/fly"),
  permanent("/docs/deployment/deployment-methods/wasp-deploy/overview", "/docs/deployment/methods/wasp-deploy/overview"),
  permanent("/docs/deployment/deployment-methods/wasp-deploy/railway",  "/docs/deployment/methods/wasp-deploy/railway"),
  permanent("/docs/deployment/intro",                                   "/docs/deployment/overview"),
  permanent("/docs/general/cli",                                        "/docs/advanced/cli"),
  permanent("/docs/general/spec",                                       "/docs/features/spec"),
  permanent("/docs/project/client-config",                              "/docs/advanced/client-customization/client-config"),
  permanent("/docs/project/custom-vite-config",                         "/docs/advanced/client-customization/custom-vite-config"),
  permanent("/docs/project/customizing-app",                            "/docs/advanced/client-customization/customizing-app"),
  permanent("/docs/project/dependencies",                               "/docs/advanced/dependencies"),
  permanent("/docs/project/env-vars",                                   "/docs/advanced/env-vars"),
  permanent("/docs/project/server-config",                              "/docs/advanced/server-customization/server-config"),
  permanent("/docs/project/starter-templates",                          "/docs/getting-started/starter-templates"),
  permanent("/docs/project/static-assets",                              "/docs/advanced/client-customization/static-assets"),
  permanent("/docs/project/testing",                                    "/docs/advanced/testing"),
  permanent("/docs/wasp-ai/coding-agent-plugin",                        "/docs/getting-started/coding-agent-plugin"),
  permanent("/docs/wasp-ai/git-worktrees",                              "/docs/advanced/git-worktrees"),
];

/** Builds a permanent redirect rule (301). */
function permanent(from: string, to: string): RedirectRule {
  return { from, to, code: 301 };
}

/** Builds a temporary redirect rule (302). */
function temporary(from: string, to: string): RedirectRule {
  return { from, to, code: 302 };
}
