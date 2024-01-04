import { User } from '@wasp/auth/types'
import { findUserIdentity, getEmail } from '@wasp/auth/user'

export function getName(user?: User) {
  if (!user) {
    return null
  }

  // We use multiple auth methods, so we need to check which one is available.
  const emailIdentity = findUserIdentity(user, 'email')
  if (emailIdentity) {
    return getEmail(user)
  }

  const googleIdentity = findUserIdentity(user, 'google')
  if (googleIdentity) {
    return `Google user ${googleIdentity.providerUserId}`
  }

  // If we don't know how to get the name, return null.
  return null
}

export function getProviderData(user?: User) {
  if (!user) {
    return null
  }

  const emailIdentity = findUserIdentity(user, 'email')
  return emailIdentity && 'isEmailVerified' in emailIdentity.providerData
    ? emailIdentity.providerData
    : null
}
