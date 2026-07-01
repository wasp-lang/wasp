export function stripTrailingSlash(value: string): string {
  return value.replace(/\/+$/, "");
}
