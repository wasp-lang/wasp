import { hash, verify, type Options } from "@node-rs/argon2";

// The options are the same as the ones used in the oslo/password library
const hashingOptions: Options = {
  memoryCost: 19456,
  timeCost: 2,
  outputLen: 32,
  parallelism: 1,
};

/**
 * Hashes a password using the Argon2 algorithm.
 * - It normalizes the password password before hashing to ensure consistency.
 * - It doesn't require a salt input as the argon2 library generates a random salt for each hash.
 *
 * @param password The password to hash
 * @returns A PHC string (e.g. $argon2id$v=19$m=19456,t=2,p=1$...) whichs contains the hashed password and the salt.
 * @see https://github.com/P-H-C/phc-string-format/blob/master/phc-sf-spec.md for more information about the PHC string format.
 */
export async function hashPassword(password: string): Promise<string> {
  return hash(normalizePassword(password), hashingOptions);
}

/**
 *
 * @param hashedPassword A PHC string (e.g. $argon2id$v=19$m=19456,t=2,p=1$...) whichs contains the hashed password and the salt.
 * @param password The password to verify
 * @throws An error if the password is invalid
 */
export async function verifyPassword(
  hashedPassword: string,
  password: string,
): Promise<void> {
  const validPassword = await verify(
    hashedPassword,
    normalizePassword(password),
    hashingOptions,
  );
  if (!validPassword) {
    throw new Error("Invalid password");
  }
}

// We are normalising the password to ensure that the password is always hashed in the same way.
function normalizePassword(password: string): string {
  return password.normalize("NFKC");
}
