import config from '../../config.js'

export const googleSignInUrl = `${config.apiUrl}/auth/external/google/login`

const containerStyle = {
  height: 40,
  border: '1px solid darkgray',
  'border-radius': 5,
  padding: 5,
  margin: '5px 0px',
  'background-color': 'white'
}

const linkStyle = {
  textDecoration: 'none',
  color: 'black'
}

const logoStyle = {
  'max-width': '100%',
  'max-height': '100%',
  display: 'inline-block',
  'margin-right': 10
}

export function GoogleSignInButton() {
  return (
    <div style={containerStyle}>
      <a href={googleSignInUrl} style={linkStyle}>
        <img alt="Google" src="/images/google-logo-icon.png" style={logoStyle} />
        <span>Log in with Google</span>
      </a>
    </div>
  )
}
