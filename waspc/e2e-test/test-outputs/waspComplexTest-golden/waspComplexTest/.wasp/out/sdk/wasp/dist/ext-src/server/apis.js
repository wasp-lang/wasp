export const fooBar = (req, res, context) => {
    res.set('Access-Control-Allow-Origin', '*');
    res.json({ msg: 'Hello, context.user.username!' });
};
export const fooBaz = (req, res, context) => {
    res.json({ msg: 'Hello, stranger!' });
};
export const fooBarMiddlewareFn = (middlewareConfig) => {
    return middlewareConfig;
};
//# sourceMappingURL=apis.js.map