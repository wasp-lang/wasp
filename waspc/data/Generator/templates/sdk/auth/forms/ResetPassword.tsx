import Auth from './Auth'
import { type CustomizationOptions, State } from './types'

export function ResetPasswordForm({
  appearance,
  logo,
  socialLayout,
}: CustomizationOptions) {
  return (
    <Auth
      appearance={appearance}
      logo={logo}
      socialLayout={socialLayout}
      state={State.ResetPassword}
    />
  )
}
