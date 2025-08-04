import { hash, verify, Version, type Options } from "@node-rs/argon2";

// The options are the same as the ones used in the oslo/password library
const hashingOptions: Options = {
  memoryCost: 19456,
  timeCost: 2,
  outputLen: 32,
  parallelism: 1,
  version: Version.V0x13,
};

// PRIVATE API
export async function hashPassword(password: string): Promise<string> {
  return hash(normalizePassword(password), hashingOptions);
}

// PRIVATE API
export async function verifyPassword(
  hashedPassword: string,
  password: string
): Promise<void> {
  const validPassword = await verify(
    hashedPassword,
    normalizePassword(password),
    hashingOptions
  );
  if (!validPassword) {
    throw new Error("Invalid password");
  }
}

// We are normalising the password to ensure that the password is always hashed in the same way
// We have the same normalising process as oslo/password did in the past
function normalizePassword(password: string): string {
  return password.normalize("NFKC");
}
