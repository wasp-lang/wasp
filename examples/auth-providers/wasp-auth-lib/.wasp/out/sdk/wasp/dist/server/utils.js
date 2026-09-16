/**
 * Simple helper to give the correct types for Express handlers.
 * We define it in the same file as our extension to Request
 * so that it is picked up by TypeScript.
 */
export const defineHandler = (middleware) => middleware;
export const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
export function redirect(res, redirectUri) {
    return res
        .status(302)
        .setHeader("Location", redirectUri)
        .end();
}
//# sourceMappingURL=utils.js.map