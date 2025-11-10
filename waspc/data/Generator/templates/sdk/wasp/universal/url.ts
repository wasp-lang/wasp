export function stripTrailingSlash(url: string): string
export function stripTrailingSlash(url: undefined): undefined
export function stripTrailingSlash(url?: string): string | undefined {
    return url?.replace(/\/$/, "");
}

export function getOrigin(url: string) {
  try {
    return new URL(url).origin;
  } catch {
    // Do we need to protect ourselves from this error, do we validate it
    return url;
  }
}
