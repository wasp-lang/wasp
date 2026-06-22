import type { ProviderIdFor } from "../authService/types";
import {
  createProviderId,
  sanitizeAndSerializeProviderData,
} from "../providerData";
import { getUsernameSignupCredentials } from "./credentials";
import type { UsernameSignupArgs, UsernameSignupResult } from "./types";

export async function signupWithUsername<
  RequestContext,
  CreatedUser,
  UserFields extends object = object,
>({
  fields,
  request,
  getUserFields,
  adapters,
}: UsernameSignupArgs<
  RequestContext,
  CreatedUser,
  UserFields
>): Promise<UsernameSignupResult> {
  const credentials = getUsernameSignupCredentials(fields);
  const userFields = await getUserFields?.(fields);
  const providerId = createProviderId(
    "username",
    credentials.username,
  ) as ProviderIdFor<"username">;
  const serializedProviderData =
    await sanitizeAndSerializeProviderData<"username">({
      hashedPassword: credentials.password,
    });

  await adapters.hooks.onBeforeSignup({ request, providerId });
  const createdUser = await adapters.authRepository.createUserWithIdentity({
    providerId,
    serializedProviderData,
    userFields,
  });
  await adapters.hooks.onAfterSignup({
    request,
    providerId,
    user: createdUser.user,
  });

  return { success: true };
}
