import { type CustomizationOptions, State } from '../../../core/auth/forms/types';
import Auth from './Auth';

// PUBLIC API
export function ForgotPasswordForm({
  appearance,
  logo,
  socialLayout,
}: CustomizationOptions): React.JSX.Element {
  return (
    <Auth
      appearance={appearance}
      logo={logo}
      socialLayout={socialLayout}
      state={State.ForgotPassword}
    />
  )
}
