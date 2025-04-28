import {
  createProviderId,
  getProviderDataWithPassword,
  updateAuthIdentityProviderData,
} from 'wasp/auth/utils'
import { VerifyAllUserEmails } from 'wasp/server/operations'

/*
  When we run headless tests on production code that doesn't auto verify emails,
  we need to manually verify all user emails.
*/
export const verifyAllUserEmails: VerifyAllUserEmails = async (
  _args,
  context
) => {
  const { User } = context.entities

  const users = await User.findMany({
    include: {
      auth: {
        include: {
          identities: true,
        },
      },
    },
  })

  for (const user of users) {
    const emailIdentity = user.auth?.identities.find(
      (identity) => identity.providerName === 'email'
    )
    if (!emailIdentity) {
      continue
    }
    const emailProviderId = createProviderId(
      'email',
      emailIdentity.providerUserId
    )
    const existingData = getProviderDataWithPassword<'email'>(
      emailIdentity.providerData
    )
    console.log('Verifying email for user:', emailProviderId.providerUserId)
    await updateAuthIdentityProviderData<'email'>(
      emailProviderId,
      existingData,
      {
        isEmailVerified: true,
      }
    )
  }
}
