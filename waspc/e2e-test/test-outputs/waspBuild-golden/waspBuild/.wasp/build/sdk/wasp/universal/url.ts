export function stripTrailingSlash(url?: string): string | undefined {
    return url?.replace(/\/$/, "");
}
