import React from 'react'
import './AuthMethodsGrid.css'

export function AuthMethodsGrid() {
  const authMethods = [
    {
      title: 'Email',
      description: 'Email verification, password reset, etc.',
      linkToDocs: '/docs/auth/email',
    },
    {
      title: 'Username & Password',
      description: 'The simplest way to get started',
      linkToDocs: '/docs/auth/username-and-pass',
    },
    {
      title: 'Google',
      description: 'Users sign in with their Google account',
      linkToDocs: '/docs/auth/social-auth/google',
    },
    {
      title: 'Github',
      description: 'Users sign in with their Github account',
      linkToDocs: '/docs/auth/social-auth/github',
    },
    {
      title: 'Keycloak',
      description: 'Users sign in with their Keycloak account',
      linkToDocs: '/docs/auth/social-auth/keycloak',
    }
  ]
  return (
    <>
      <div className="auth-methods-grid">
        {authMethods.map((authMethod) => (
          <AuthMethodBox
            title={authMethod.title}
            description={authMethod.description}
            linkToDocs={authMethod.linkToDocs}
          />
        ))}
      </div>
      <p className="auth-methods-info">
        <small>Click on each auth method for more details.</small>
      </p>
    </>
  )
}

function AuthMethodBox({
  linkToDocs,
  title,
  description,
}: {
  linkToDocs: string
  title: string
  description: string
}) {
  return (
    <a href={linkToDocs} className="auth-method-box">
      <h3>{title} »</h3>
      <p>{description}</p>
    </a>
  )
}
