// PUBLIC API
// NOTE: This is enough to cover Operations and our APIs (src/Wasp/AppSpec/Api.hs).
export var HttpMethod;
(function (HttpMethod) {
    HttpMethod["Get"] = "GET";
    HttpMethod["Post"] = "POST";
    HttpMethod["Put"] = "PUT";
    HttpMethod["Delete"] = "DELETE";
})(HttpMethod || (HttpMethod = {}));
// PUBLIC API
export { config } from './config.js';
// PUBLIC API
export { env } from './env.js';
//# sourceMappingURL=index.js.map