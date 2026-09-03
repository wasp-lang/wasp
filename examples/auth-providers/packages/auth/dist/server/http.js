/**
 * The same wire shape Wasp's `HttpError` produces (`{ message, data }` with
 * the status code), so the package's forms and any user code reading auth
 * errors see exactly what the in-tree flows produced. Duck-typed on purpose:
 * the app's own `HttpError` class (thrown from user hooks) is a different
 * class identity, and `statusCode` is what both share.
 */
export class HttpError extends Error {
    statusCode;
    data;
    constructor(statusCode, message, data) {
        super(message);
        this.name = "HttpError";
        this.statusCode = statusCode;
        if (data) {
            this.data = data;
        }
    }
}
export function isHttpErrorLike(error) {
    return (typeof error === "object" &&
        error !== null &&
        typeof error.statusCode === "number");
}
export function json(res, status, payload) {
    res.statusCode = status;
    res.setHeader("Content-Type", "application/json");
    res.end(JSON.stringify(payload));
}
export function redirect(res, location) {
    res.statusCode = 302;
    res.setHeader("Location", location);
    res.end();
}
export function getBody(req) {
    const body = req.body;
    return typeof body === "object" && body !== null
        ? body
        : {};
}
export function getUrl(req) {
    return new URL(req.url ?? "/", "http://placeholder");
}
/**
 * A minimal dispatcher over the routes mounted at the manifest's basePath.
 * Errors follow Wasp's error-handling contract: anything carrying a
 * `statusCode` (this package's `HttpError`, the app's own `HttpError`
 * thrown from a hook) answers with that status and `{ message, data }`;
 * everything else is a logged 500.
 */
export function makeDispatcher(routes) {
    return async (req, res) => {
        const url = getUrl(req);
        const route = routes.find((r) => r.method === req.method && r.path === url.pathname);
        if (route === undefined) {
            json(res, 404, { message: "Not found." });
            return;
        }
        try {
            await route.handler(req, res);
        }
        catch (error) {
            if (isHttpErrorLike(error)) {
                json(res, error.statusCode, {
                    message: error.message,
                    ...(error.data !== undefined ? { data: error.data } : {}),
                });
                return;
            }
            console.error(error);
            json(res, 500, { message: "Internal server error" });
        }
    };
}
