{{={= =}=}}
import { HttpError } from 'wasp/server'
import {
  type ProviderId,
  createUser,
  sanitizeAndSerializeProviderData,
  validateAndGetUserFields,
  createProviderId,
} from 'wasp/auth/utils'
import { type {= authEntityUpper =} } from 'wasp/entities'
import { prisma } from 'wasp/server'
import { type UserSignupFields, type ProviderConfig } from 'wasp/auth/providers/types'
import { getRedirectUriForOneTimeCode, getRedirectUriForError } from './redirect'
import { tokenStore } from './oneTimeCode'

export async function finishOAuthFlowAndGetRedirectUri(
  provider: ProviderConfig,
  providerProfile: unknown,
  providerUserId: string,
  userSignupFields: UserSignupFields | undefined,
): Promise<URL> {
  const providerId = createProviderId(provider.id, providerUserId);

  const authId = await getAuthIdFromProviderDetails(providerId, providerProfile, userSignupFields);

  const oneTimeCode = await tokenStore.createToken(authId);

  return getRedirectUriForOneTimeCode(oneTimeCode);
}

export function handleOAuthErrorAndGetRedirectUri(error: unknown): URL {
  if (error instanceof HttpError) {
    const errorMessage = isHttpErrorWithExtraMessage(error)
      ? `${error.message}: ${error.data.message}`
      : error.message;
    return getRedirectUriForError(errorMessage)
  }
  console.error("Unknown OAuth error:", error);
  return getRedirectUriForError("An unknown error occurred while trying to log in with the OAuth provider.");
}

function isHttpErrorWithExtraMessage(error: HttpError): error is HttpError & { data: { message: string } } {
  return error.data && typeof (error.data as any).message === 'string';
}

// We need a user id to create the auth token, so we either find an existing user
// or create a new one if none exists for this provider.
async function getAuthIdFromProviderDetails(
  providerId: ProviderId,
  providerProfile: any,
  userSignupFields: UserSignupFields | undefined,
): Promise<{= authEntityUpper =}['id']> {
  const existingAuthIdentity = await prisma.{= authIdentityEntityLower =}.findUnique({
    where: {
      providerName_providerUserId: providerId,
    },
    include: {
      {= authFieldOnAuthIdentityEntityName =}: {
        include: {
          {= userFieldOnAuthEntityName =}: true
        }
      }
    }
  })

  if (existingAuthIdentity) {
    return existingAuthIdentity.{= authFieldOnAuthIdentityEntityName =}.id
  } else {
    const userFields = await validateAndGetUserFields(
      { profile: providerProfile },
      userSignupFields,
    );

    // For now, we don't have any extra data for the oauth providers, so we just pass an empty object.
    const providerData = await sanitizeAndSerializeProviderData({})
  
    const user = await createUser(
      providerId,
      providerData,
      // Using any here because we want to avoid TypeScript errors and
      // rely on Prisma to validate the data.
      userFields as any,
    )

    return user.auth.id
  }
}
