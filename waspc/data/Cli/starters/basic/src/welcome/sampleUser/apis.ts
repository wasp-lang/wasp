// Development-only helper for creating a ready-to-use user.

import {
  createProviderId,
  createUser,
  findAuthIdentity,
  sanitizeAndSerializeProviderData,
} from "wasp/server/auth";
import { type CreateSampleUser } from "wasp/server/api";

export type SampleUser = {
  email: string;
  password: string;
};

const sampleUser: SampleUser = {
  email: "sample@example.com",
  password: "password123",
};

const sampleUserProviderId = createProviderId("email", sampleUser.email);

// Safe to call more than once: if the user already exists, it just returns the
// credentials again.
export const createSampleUser: CreateSampleUser = async (_req, res) => {
  const authIdentity = await findAuthIdentity(sampleUserProviderId);
  if (!authIdentity) {
    const providerData = await sanitizeAndSerializeProviderData<"email">({
      hashedPassword: sampleUser.password,
      // Skips email verification, so the user can log in right away.
      isEmailVerified: true,
      emailVerificationSentAt: null,
      passwordResetSentAt: null,
    });
    await createUser(sampleUserProviderId, providerData, {
      username: "sampleuser",
    });
  }

  res.json(sampleUser);
};
