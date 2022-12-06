import config from '../../config.js'

export const githubSignInUrl = `${config.apiUrl}/auth/external/github/login`

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

export function GitHubSignInButton() {
  return (
    <div style={containerStyle}>
      <a href={githubSignInUrl} style={linkStyle}>
        <img alt="GitHub" src="/images/github-logo-icon.png" style={logoStyle} />
        <span>Log in with GitHub</span>
      </a>
    </div>
  )
}
