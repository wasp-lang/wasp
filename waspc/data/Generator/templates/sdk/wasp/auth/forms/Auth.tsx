{{={= =}=}}
import { useMemo } from 'react'
import styles from './Auth.module.css'
import './internal/auth-styles.css'
import { tokenObjToCSSVars } from "./internal/util"

import {
  type State,
  type CustomizationOptions,
  type AdditionalSignupFields,
} from './types'
import { LoginSignupForm } from './internal/common/LoginSignupForm'
{=# isEmailAuthEnabled =}
import { ForgotPasswordForm } from './internal/email/ForgotPasswordForm'
import { ResetPasswordForm } from './internal/email/ResetPasswordForm'
import { VerifyEmailForm } from './internal/email/VerifyEmailForm'
{=/ isEmailAuthEnabled =}

const logoStyle = {
  height: '3rem'
}


function Auth ({ state, appearance, logo, socialLayout = 'horizontal', additionalSignupFields }: {
    state: State;
} & CustomizationOptions & {
  additionalSignupFields?: AdditionalSignupFields;
}) {
  const customStyle = useMemo(() => ({
    ...tokenObjToCSSVars('color', appearance?.colors ?? {}),
    ...tokenObjToCSSVars('font-size', appearance?.fontSizes ?? {}),
  }), [appearance])

  const titles: Record<State, string> = {
    login: 'Log in to your account',
    signup: 'Create a new account',
    {=# isEmailAuthEnabled =}
    "forgot-password": "Forgot your password?",
    "reset-password": "Reset your password",
    "verify-email": "Email verification",
    {=/ isEmailAuthEnabled =}
  }
  const title = titles[state]

  const socialButtonsDirection = socialLayout === 'vertical' ? 'vertical' : 'horizontal'

  return (
    <div className={styles.container} style={customStyle}>
      <div>
        {logo && (<img style={logoStyle} src={logo} alt='Your Company' />)}
        <h2 className={styles.headerText}>{title}</h2>
      </div>

      {(state === 'login' || state === 'signup') && (
        <LoginSignupForm
          state={state}
          socialButtonsDirection={socialButtonsDirection}
          additionalSignupFields={additionalSignupFields}
        />
      )}
      {=# isEmailAuthEnabled =}
      {state === 'forgot-password' && (<ForgotPasswordForm />)}
      {state === 'reset-password' && (<ResetPasswordForm />)}
      {state === 'verify-email' && (<VerifyEmailForm />)}
      {=/ isEmailAuthEnabled =}
    </div>
  )
}

// PRIVATE API
export default Auth;
