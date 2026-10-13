// Development-only helper for creating a ready-to-use user.
//
// Once you no longer need it, delete the whole `src/welcome/sampleUser/`
// folder, and remove `sampleUserSpec` from `src/welcome/welcome.wasp.ts` and
// `<SampleUserButton />` from `src/welcome/LogInStep.tsx`.

// Usually you'd type an API handler with the type Wasp generates for it, like
// `import type { CreateSampleUser } from "wasp/server/api"`. This endpoint only
// exists in development, so that type is missing from production builds, and we
// use Express' types (plus an unused `context` argument) instead.
import type { Request, Response } from "express";
import {
  createProviderId,
  createUser,
  findAuthIdentity,
  sanitizeAndSerializeProviderData,
} from "wasp/server/auth";

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
export async function createSampleUser(
  _req: Request,
  res: Response,
  _context: unknown,
) {
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
}
