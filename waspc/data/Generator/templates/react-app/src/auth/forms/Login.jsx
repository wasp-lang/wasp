{{={= =}=}}
import Auth from './Auth'

const LoginForm = ({ appearance, logo, socialLayout }) => {
  return (
    <Auth
      appearance={appearance}
      logo={logo}
      socialLayout={socialLayout}
      isLogin={true}
    />
  )
}

export default LoginForm
