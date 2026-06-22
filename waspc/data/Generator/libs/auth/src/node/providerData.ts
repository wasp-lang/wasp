import { hashPassword } from "./password";

export type EmailProviderData = {
  hashedPassword: string;
  isEmailVerified: boolean;
  emailVerificationSentAt: string | null;
  passwordResetSentAt: string | null;
};

export type UsernameProviderData = {
  hashedPassword: string;
};

export type OAuthProviderData = Record<string, never>;

export type PossibleProviderData = {
  email: EmailProviderData;
  username: UsernameProviderData;
  discord: OAuthProviderData;
  slack: OAuthProviderData;
  google: OAuthProviderData;
  keycloak: OAuthProviderData;
  github: OAuthProviderData;
  microsoft: OAuthProviderData;
};

export type ProviderName = keyof PossibleProviderData;

export type ProviderId = {
  providerName: ProviderName;
  providerUserId: string;
};

export function createProviderId(
  providerName: ProviderName,
  providerUserId: string,
): ProviderId {
  return {
    providerName,
    providerUserId: normalizeProviderUserId(providerName, providerUserId),
  };
}

export function normalizeProviderUserId(
  providerName: ProviderName,
  providerUserId: string,
): string {
  switch (providerName) {
    case "email":
    case "username":
      return providerUserId.toLowerCase();
    case "google":
    case "github":
    case "discord":
    case "keycloak":
    case "slack":
    case "microsoft":
      return providerUserId;
    default:
      providerName satisfies never;
      return providerUserId;
  }
}

export function getProviderData<PN extends ProviderName>(
  providerData: string,
): Omit<PossibleProviderData[PN], "hashedPassword"> {
  return sanitizeProviderData(getProviderDataWithPassword(providerData));
}

export function getProviderDataWithPassword<PN extends ProviderName>(
  providerData: string,
): PossibleProviderData[PN] {
  return JSON.parse(providerData) as PossibleProviderData[PN];
}

export async function sanitizeAndSerializeProviderData<
  PN extends ProviderName,
>(providerData: PossibleProviderData[PN]): Promise<string> {
  return serializeProviderData(await ensurePasswordIsHashed(providerData));
}

export async function mergeAndSerializeProviderDataUpdates<
  PN extends ProviderName,
>(
  existingProviderData: PossibleProviderData[PN],
  providerDataUpdates: Partial<PossibleProviderData[PN]>,
): Promise<string> {
  const sanitizedProviderDataUpdates = await ensurePasswordIsHashed(
    providerDataUpdates,
  );
  return serializeProviderData({
    ...existingProviderData,
    ...sanitizedProviderDataUpdates,
  });
}

function sanitizeProviderData<PN extends ProviderName>(
  providerData: PossibleProviderData[PN],
): Omit<PossibleProviderData[PN], "hashedPassword"> {
  if (providerDataHasPasswordField(providerData)) {
    const { hashedPassword: _hashedPassword, ...rest } = providerData;
    return rest;
  }
  return providerData;
}

function serializeProviderData<PN extends ProviderName>(
  providerData: PossibleProviderData[PN],
): string {
  return JSON.stringify(providerData);
}

async function ensurePasswordIsHashed<
  ProviderData extends Partial<PossibleProviderData[ProviderName]>,
>(providerData: ProviderData): Promise<ProviderData> {
  const data = { ...providerData };

  if (providerDataHasPasswordField(data)) {
    data.hashedPassword = await hashPassword(data.hashedPassword);
  }

  return data;
}

function providerDataHasPasswordField(
  providerData: Partial<PossibleProviderData[ProviderName]>,
): providerData is { hashedPassword: string } {
  return (
    "hashedPassword" in providerData &&
    typeof providerData.hashedPassword === "string"
  );
}
