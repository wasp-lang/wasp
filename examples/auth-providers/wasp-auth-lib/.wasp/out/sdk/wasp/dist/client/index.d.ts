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
export { ClientConfig, config } from './config';
export { env } from './env';
//# sourceMappingURL=index.d.ts.map