{{={= =}=}}
// @ts-nocheck
import { lazy, Suspense, createElement } from 'react';
{=# isAuthEnabled =}
import { createAuthRequiredPage } from "wasp/client/app"
{=/ isAuthEnabled =}

// Static imports for SSR-enabled pages (included in the SSR bundle):
{=# ssrPagesToImport =}
{=& importStatement =}
{=/ ssrPagesToImport =}

function getOptionalExport(mod, exportName) {
  return mod && typeof mod === "object" ? mod[exportName] : undefined;
}

// Helper: wraps a dynamic import() in React.lazy + Suspense so the page
// is code-split and excluded from the SSR bundle.
function createLazyPage(loader) {
  const LazyComponent = lazy(loader);
  return function LazyPage(props) {
    return createElement(Suspense, { fallback: null }, createElement(LazyComponent, props));
  };
}

// Lazy imports for non-SSR pages (excluded from the SSR bundle):
{=# nonSsrPagesToImport =}
const {= moduleIdentifier =} = { {= exportedName =}: createLazyPage(() => import('{= importPath =}').then(m => ({ default: m.{= exportedName =} }))) };
{=/ nonSsrPagesToImport =}

export const routesMapping = {
  {=# routes =}
  {= name =}: {= targetComponent =},
  {=/ routes =}
} as const;

export const routeNameToSsr = {
  {=# routes =}
  {= name =}: {=# ssr =}true{=/ ssr =}{=^ ssr =}false{=/ ssr =},
  {=/ routes =}
} as const;

export const routeNameToHead = {
  {=# routes =}
  {= name =}: getOptionalExport({= pageModuleIdentifier =}, "head"),
  {=/ routes =}
} as const;
