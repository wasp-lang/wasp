import Auth from './Auth'
import { type CustomizationOptions, State, AdditionalSignupFields, AdditionalSignupFieldsRender } from './types'

export function SignupForm({
  appearance,
  logo,
  socialLayout,
  additionalFields,
  additionalFieldsRender,
}: CustomizationOptions & { additionalFields?: AdditionalSignupFields; additionalFieldsRender?: AdditionalSignupFieldsRender }) {
  return (
    <Auth
      appearance={appearance}
      logo={logo}
      socialLayout={socialLayout}
      state={State.Signup}
      additionalSignupFields={additionalFields}
      additionalSignupFieldsRender={additionalFieldsRender}
    />
  )
}
