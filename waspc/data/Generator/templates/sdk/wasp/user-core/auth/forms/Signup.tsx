import { type CustomizationOptions, State } from '../../../core/auth/forms/types';
import Auth from './Auth';
import {
  type AdditionalSignupFields,
} from './types';

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
