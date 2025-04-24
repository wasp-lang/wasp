import { interpolatePath } from './linkHelpers';
// PUBLIC API
export const routes = {
    RootRoute: {
        to: "/",
        build: (options) => interpolatePath("/", undefined, options?.search, options?.hash),
    },
};
// PUBLIC API
export { Link } from './Link';
//# sourceMappingURL=index.js.map