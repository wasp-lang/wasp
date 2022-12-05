import config from '../../config.js'

export const githubSignInUrl = `${config.apiUrl}/auth/external/github/login`

export function GithubSignInButton(props) {
  return (
    <a href={githubSignInUrl}>
      <img alt="Sign in with GitHub" height={props?.height || 40} src="/images/btn_github_signin_dark_normal_web@2x.png" />
    </a>
  )
}
