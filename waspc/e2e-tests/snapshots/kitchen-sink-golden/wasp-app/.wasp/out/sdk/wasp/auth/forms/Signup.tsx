import Auth from './Auth'
import {
  type CustomizationOptions,
  type AdditionalSignupFields,
  State,
} from './types'

// PUBLIC API
export function SignupForm({
  appearance,
  logo,
  socialLayout,
  additionalFields,
}: CustomizationOptions & { additionalFields?: AdditionalSignupFields; }): React.JSX.Element {
  return (
    <Auth
      appearance={appearance}
      logo={logo}
      socialLayout={socialLayout}
      state={State.Signup}
      additionalSignupFields={additionalFields}
    />
  )
}
