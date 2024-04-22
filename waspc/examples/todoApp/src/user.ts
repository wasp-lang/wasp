import { type AuthUser } from 'wasp/auth'

export function getName(user?: AuthUser) {
  if (!user) {
    return null
  }

  // We use multiple auth methods, so we need to check which one is available.
  if (user.identities.email !== null) {
    return user.identities.email.id
  }

  if (user.identities.google !== null) {
    return `Google user ${user.identities.google.id}`
  }

  if (user.identities.github !== null) {
    return `GitHub user ${user.identities.github.id}`
  }

  // if (user.identities.keycloak !== undefined) {
  //   return `Keycloak user ${user.identities.keycloak.id}`
  // }

  // If we don't know how to get the name, return null.
  return null
}
