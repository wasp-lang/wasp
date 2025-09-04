export function stripTrailingSlash(url: string): string
export function stripTrailingSlash(url: undefined): undefined
export function stripTrailingSlash(url?: string): string | undefined {
    return url?.replace(/\/$/, "");
}
