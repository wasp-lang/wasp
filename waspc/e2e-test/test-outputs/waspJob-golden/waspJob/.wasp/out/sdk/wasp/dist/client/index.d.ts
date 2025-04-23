export declare enum HttpMethod {
    Get = "GET",
    Post = "POST",
    Put = "PUT",
    Delete = "DELETE"
}
export type Route = {
    method: HttpMethod;
    path: string;
};
export { config, ClientConfig } from './config.js';
export { env } from './env.js';
//# sourceMappingURL=index.d.ts.map