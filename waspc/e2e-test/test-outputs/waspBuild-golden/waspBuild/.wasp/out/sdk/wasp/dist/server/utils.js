/**
 * Decorator for async express middleware that handles promise rejections.
 * @param {Func} middleware - Express middleware function.
 * @returns Express middleware that is exactly the same as the given middleware but,
 *   if given middleware returns promise, reject of that promise will be correctly handled,
 *   meaning that error will be forwarded to next().
 */
export const handleRejection = (middleware) => async (req, res, next) => {
    try {
        await middleware(req, res, next);
    }
    catch (error) {
        next(error);
    }
};
export const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
//# sourceMappingURL=utils.js.map