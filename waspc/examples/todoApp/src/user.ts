import { getEmail, findUserIdentity, type AuthUser as User } from 'wasp/auth'

export function getName(user?: User) {
  if (!user) {
    return null
  }

  // We use multiple auth methods, so we need to check which one is available.
  const emailIdentity = findUserIdentity(user, 'email')
  if (emailIdentity !== undefined) {
    return getEmail(user)
  }

  const googleIdentity = findUserIdentity(user, 'google')
  if (googleIdentity !== undefined) {
    return `Google user ${googleIdentity.providerUserId}`
  }

  const githubIdentity = findUserIdentity(user, 'github')
  if (githubIdentity !== undefined) {
    return `GitHub user ${githubIdentity.providerUserId}`
  }

  const keycloakIdentity = findUserIdentity(user, 'keycloak')
  if (keycloakIdentity) {
    return `Keycloak user ${keycloakIdentity.providerUserId}`
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
