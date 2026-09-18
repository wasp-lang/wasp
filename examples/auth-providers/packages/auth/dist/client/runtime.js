/**
 * The client auth handler's window into the app, captured when Wasp instantiates
 * the handler. Forms and actions read it; nothing here imports generated
 * code.
 */
let runtime = null;
let config = null;
export function setClientState(newRuntime, newConfig) {
    runtime = newRuntime;
    config = newConfig;
}
export function getClientRuntime() {
    if (runtime === null) {
        throw new Error("Wasp's auth client used before Wasp instantiated it. Is waspAuth() among app.auth.schemes?");
    }
    return runtime;
}
export function getClientConfig() {
    if (config === null) {
        throw new Error("Wasp's auth client used before Wasp instantiated it. Is waspAuth() among app.auth.schemes?");
    }
    return config;
}
