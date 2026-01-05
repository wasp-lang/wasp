{{={= =}=}}
export function getRoutesTsxContent(): string {
  return `{=# isAuthEnabled =}import { createAuthRequiredPage } from "wasp/client/auth";

{=/ isAuthEnabled =}{=# pagesToImport =}{=& importStatement =}
{=/ pagesToImport =}
export const routes = {
{=# routes =}  {= name =}: {= targetComponent =},
{=/ routes =}} as const;
`;
}
