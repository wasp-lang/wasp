import assert from "node:assert";
import { waspSays } from "./terminal.js";
const DEFAULT_MAX_ATTEMPTS = 3;
export async function retryOnTransientError(fn, options) {
    const maxAttempts = options.maxAttempts ?? DEFAULT_MAX_ATTEMPTS;
    assert(maxAttempts >= 1, "maxAttempts must be at least 1");
    const reason = options.retryDescription ?? "a transient error";
    let lastError;
    for (let attempt = 1; attempt <= maxAttempts; attempt++) {
        try {
            return await fn();
        }
        catch (error) {
            lastError = error;
            if (!options.isRetryable(error)) {
                throw error;
            }
            if (attempt < maxAttempts) {
                waspSays(`Command failed due to ${reason}, retrying (attempt ${attempt + 1}/${maxAttempts})...`);
            }
        }
    }
    throw lastError;
}
//# sourceMappingURL=retry.js.map