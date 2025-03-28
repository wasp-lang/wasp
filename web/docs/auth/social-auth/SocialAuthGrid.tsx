import React from 'react'
import { LinkGrid } from '../../../src/components/LinkGrid'

export function SocialAuthGrid({
  pagePart = '', // e.g. #overrides
}) {
  const authMethods = [
    {
      title: 'Google',
      description: 'Users sign in with their Google account.',
      link: '/docs/auth/social-auth/google' + pagePart,
    },
    {
      title: 'Github',
      description: 'Users sign in with their Github account.',
      link: '/docs/auth/social-auth/github' + pagePart,
    },
    {
      title: 'Keycloak',
      description: 'Users sign in with their Keycloak account.',
      link: '/docs/auth/social-auth/keycloak' + pagePart,
    },
    {
      title: 'Discord',
      description: 'Users sign in with their Discord account.',
      link: '/docs/auth/social-auth/discord' + pagePart,
    },
  ]

  return (
    <LinkGrid
      links={authMethods}
      caption="Click on each provider for more details."
    />
  )
}
