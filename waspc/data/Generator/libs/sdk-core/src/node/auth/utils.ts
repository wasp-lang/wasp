import { hashPassword } from "@wasp.sh/lib-auth/node";
import {
  providerDataHasPasswordField,
  type PossibleProviderData,
  type ProviderName,
} from "../../auth/providerData.js";
import { HttpError } from "../../HttpError.js";
import { sleep } from "../../utils/sleep.js";

// PRIVATE API
// If an user exists, we don't want to leak information
// about it. Pretending that we're doing some work
// will make it harder for an attacker to determine
// if a user exists or not.
// NOTE: Attacker measuring time to response can still determine
// if a user exists or not. We'll be able to avoid it when
// we implement e-mail sending via jobs.
export async function doFakeWork(): Promise<unknown> {
  const timeToWork = Math.floor(Math.random() * 1000) + 1000;
  return sleep(timeToWork);
}

export async function sanitizeAndSerializeProviderData<PN extends ProviderName>(
  providerData: PossibleProviderData[PN],
): Promise<string> {
  return serializeProviderData(await ensurePasswordIsHashed(providerData));
}

function serializeProviderData<PN extends ProviderName>(
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

export function createInvalidCredentialsError(message?: string): HttpError {
  return new HttpError(401, "Invalid credentials", { message });
}
