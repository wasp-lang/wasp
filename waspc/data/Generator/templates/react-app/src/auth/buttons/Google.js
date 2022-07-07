// TODO: Fix hardcoded URL.

export default function Google(props) {
  return (
    <a href="http://localhost:3001/auth/external/google/login">
      <img alt="Google" height={props?.height || 40} src="/images/btn_google_signin_dark_normal_web@2x.png" />
    </a>
  )
}
