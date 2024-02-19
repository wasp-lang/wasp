import Auth from './Auth'
import { type CustomizationOptions, State } from './types'

// PUBLIC API
export function LoginForm({
  appearance,
  logo,
  socialLayout,
}: CustomizationOptions) {
  return (
    <Auth
      appearance={appearance}
      logo={logo}
      socialLayout={socialLayout}
      state={State.Login}
    />
  )
}
