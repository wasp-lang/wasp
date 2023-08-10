{{={= =}=}}
export type Routes = 
  {=# routes =}
| {to: "{= urlPath =}", params{=^ urlParams =}?{=/ urlParams =}: {{=# urlParams =}{= . =}: string | number;{=/ urlParams =}}}
  {=/ routes =}
| never

export const routes = {
  {=# routes =}
  {=# hasUrlParams =}
  {= name =}: (params: {{=# urlParams =}{= . =}: string | number;{=/ urlParams =}}) => interpolatePath("{= urlPath =}", params),
  {=/ hasUrlParams =}
  {=^ hasUrlParams =}
  {= name =}: () => "{= urlPath =}",
  {=/ hasUrlParams =}
  {=/ routes =}
}

export function interpolatePath(path: string, params: Record<string, string | number>) {
  return path.split('/').map((part) => {
    if (part.startsWith(':')) {
      const paramName = part.slice(1)
      return params[paramName]
    }
    return part
  }).join('/')
}
