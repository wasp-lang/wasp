import { hashPassword } from "@wasp.sh/lib-auth/node";
import {
  providerDataHasPasswordField,
  type PossibleProviderData,
  type ProviderName,
} from "../../auth/providerData.js";
import { HttpError } from "../../HttpError.js";
import { sleep } from "../../utils/sleep.js";

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
