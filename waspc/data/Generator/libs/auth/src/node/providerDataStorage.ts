import {
  providerDataHasPasswordField,
  type PossibleProviderData,
  type ProviderName,
} from "../providerData.js";
import { hashPassword } from "./password.js";

/**
 * Prepares provider data for storage: hashes the password (if there is one)
 * and serializes the result to a JSON string.
 */
export async function sanitizeAndSerializeProviderData<PN extends ProviderName>(
  providerData: PossibleProviderData[PN],
): Promise<string> {
  return serializeProviderData(await ensurePasswordIsHashed(providerData));
}

export function serializeProviderData<PN extends ProviderName>(
  providerData: PossibleProviderData[PN],
): string {
  return JSON.stringify(providerData);
}

export async function ensurePasswordIsHashed<PN extends ProviderName>(
  providerData: PossibleProviderData[PN],
): Promise<PossibleProviderData[PN]> {
  const data = {
    ...providerData,
  };
  if (providerDataHasPasswordField(data)) {
    data.hashedPassword = await hashPassword(data.hashedPassword);
  }

  return data;
}
