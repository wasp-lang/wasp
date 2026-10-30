import crypto from "crypto";
export function generateRandomHexString(sizeBytes = 32) {
    return crypto.randomBytes(sizeBytes).toString("hex");
}
//# sourceMappingURL=random.js.map