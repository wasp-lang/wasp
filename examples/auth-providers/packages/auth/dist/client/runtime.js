/**
 * The client adapter's window into the app, captured when Wasp instantiates
 * the adapter. Forms and actions read it; nothing here imports generated
 * code.
 */
let runtime = null;
let options = null;
export function setClientState(newRuntime, newOptions) {
    runtime = newRuntime;
    options = newOptions;
}
export function getClientRuntime() {
    if (runtime === null) {
        throw new Error("Wasp's auth client used before Wasp instantiated it. Is waspAuth() among app.auth.providers?");
    }
    return runtime;
}
export function getClientOptions() {
    if (options === null) {
        throw new Error("Wasp's auth client used before Wasp instantiated it. Is waspAuth() among app.auth.providers?");
    }
    return options;
}
