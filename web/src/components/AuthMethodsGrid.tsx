import React from 'react'
import { LinkGrid } from './LinkGrid'

const authMethods = [
  {
    title: 'Email',
    description: 'Email verification, password reset, etc.',
    link: '/docs/auth/email',
  },
  {
    title: 'Username & Password',
    description: 'The simplest way to get started',
    link: '/docs/auth/username-and-pass',
  },
  {
    title: 'Google',
    description: 'Users sign in with their Google account',
    link: '/docs/auth/social-auth/google',
  },
  {
    title: 'Github',
    description: 'Users sign in with their Github account',
    link: '/docs/auth/social-auth/github',
  },
  {
    title: 'Keycloak',
    description: 'Users sign in with their Keycloak account',
    link: '/docs/auth/social-auth/keycloak',
  },
  {
    title: 'Discord',
    description: 'Users sign in with their Discord account',
    link: '/docs/auth/social-auth/discord',
  },
]

export function AuthMethodsGrid() {
  return (
    <LinkGrid
      caption="Click on each auth method for more details."
      links={authMethods}
    />
  )
}
