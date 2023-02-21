{{={= =}=}}

import config from '../../config.js'

export const signInUrl = `${config.apiUrl}{= signInPath =}`
export const logoUrl = '/images/{= iconName =}'

const containerStyle = {
  border: '2px solid rgb(107 114 128)',
  margin: 0,
  cursor: 'pointer',
  borderRadius: '.375rem',
  backgroundColor: 'rgb(255 255 255)',
  paddingLeft: '1.5rem',
  paddingRight: '1.5rem',
  paddingTop: '.75rem',
  paddingBottom: '.75rem',
  fontWeight: 600,
  color: 'rgb(17 24 39)',
  boxShadow: '0 0 #0000,0 0 #0000,0 1px 3px 0 rgb(0 0 0/0.1),0 1px 2px -1px rgb(0 0 0/0.1)',
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
