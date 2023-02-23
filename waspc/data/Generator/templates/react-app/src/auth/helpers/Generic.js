{{={= =}=}}

import config from '../../config.js'

export const signInUrl = `${config.apiUrl}{= signInPath =}`
export const logoUrl = '/images/{= iconName =}'

const containerStyle = {
  border: '2px solid #cbd5e1',
  margin: 0,
  cursor: 'pointer',
  borderRadius: '.375rem',
  backgroundColor: '#f8fafc',
  paddingLeft: '1.5rem',
  paddingRight: '1.5rem',
  paddingTop: '.75rem',
  paddingBottom: '.75rem',
  fontWeight: 600,
  color: '#1e293b',
  boxShadow: '0 1px 3px 0 rgba(0, 0, 0, 0.1), 0 1px 2px -1px rgba(0, 0, 0, 0.1)',
  outline: '2px solid transparent',
  outlineOffset: '2px',
}

const linkStyle = {
  display: 'flex',
  alignItems: 'center',
  textDecoration: 'none',
  color: 'black'
}

const logoStyle = {
  maxHeight: '24px',
  marginRight: '0.75rem'
}

export function SignInButton() {
  return (
    <div style={containerStyle}>
      <a href={signInUrl} style={linkStyle}>
        <img alt="{= displayName =} Icon" src={logoUrl} style={logoStyle} />
        <span>Log in with {= displayName =}</span>
      </a>
    </div>
  )
}
