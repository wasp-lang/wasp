import { hash, verify } from "@node-rs/argon2";
// The options are the same as the ones used in the oslo/password library
const hashingOptions = {
    memoryCost: 19456,
    timeCost: 2,
    outputLen: 32,
    parallelism: 1,
    version: 1 /* Version.V0x13 */,
};
// PRIVATE API
export async function hashPassword(password) {
    return hash(normalizePassword(password), hashingOptions);
}
// PRIVATE API
export async function verifyPassword(hashedPassword, password) {
    const validPassword = await verify(hashedPassword, normalizePassword(password), hashingOptions);
    if (!validPassword) {
        throw new Error("Invalid password");
    }
}
// We are normalising the password to ensure that the password is always hashed in the same way
// We have the same normalising process as oslo/password did in the past
function normalizePassword(password) {
    return password.normalize("NFKC");
}
//# sourceMappingURL=password.js.map