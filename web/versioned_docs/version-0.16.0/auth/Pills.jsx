import React from 'react'
import './Pills.css'
import Link from '@docusaurus/Link'

export function Pill({ children, linkToPage, style = {} }) {
  return (
    <Link
      to={linkToPage}
      style={{
        padding: '0.1rem 0.5rem',
        borderRadius: '0.375rem',
        color: 'var(--auth-pills-color)',
        textDecoration: 'none',
        display: 'inline-block',
        ...style,
      }}
    >
      {children}
    </Link>
  )
}

export function EmailPill() {
  return (
    <Pill
      style={{
        backgroundColor: 'var(--auth-pills-email)',
      }}
      linkToPage="/docs/auth/email"
    >
      Email
    </Pill>
  )
}

export function UsernameAndPasswordPill() {
  return (
    <Pill
      style={{
        backgroundColor: 'var(--auth-pills-username-and-pass)',
      }}
      linkToPage="/docs/auth/username-and-pass"
    >
      Username & Password
    </Pill>
  )
}

export function DiscordPill() {
  return (
    <Pill
      style={{
        backgroundColor: 'var(--auth-pills-discord)',
      }}
      linkToPage="/docs/auth/social-auth/discord"
    >
      Discord
    </Pill>
  )
}

export function GithubPill() {
  return (
    <Pill
      style={{
        backgroundColor: 'var(--auth-pills-github)',
      }}
      linkToPage="/docs/auth/social-auth/github"
    >
      Github
    </Pill>
  )
}

export function GooglePill() {
  return (
    <Pill
      style={{
        backgroundColor: 'var(--auth-pills-google)',
      }}
      linkToPage="/docs/auth/social-auth/google"
    >
      Google
    </Pill>
  )
}

export function KeycloakPill() {
  return (
    <Pill
      style={{
        backgroundColor: 'var(--auth-pills-keycloak)',
      }}
      linkToPage="/docs/auth/social-auth/keycloak"
    >
      Keycloak
    </Pill>
  )
}
