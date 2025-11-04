import { useState, createContext, useMemo } from 'react'
import styles from './Auth.module.css'
import './internal/auth-styles.css'
import { tokenObjToCSSVars } from "./internal/util"
import { CSSProperties } from "react"

import {
  type State,
  type CustomizationOptions,
  type ErrorMessage,
  type AdditionalSignupFields,
} from './types'
import { LoginSignupForm } from './internal/common/LoginSignupForm'
import { MessageError, MessageSuccess } from './internal/Message'
import { ForgotPasswordForm } from './internal/email/ForgotPasswordForm'
import { ResetPasswordForm } from './internal/email/ResetPasswordForm'
import { VerifyEmailForm } from './internal/email/VerifyEmailForm'

const logoStyle = {
  height: '3rem'
}


// PRIVATE API
export const AuthContext = createContext({
  isLoading: false,
  setIsLoading: (isLoading: boolean) => {},
  setErrorMessage: (errorMessage: ErrorMessage | null) => {},
  setSuccessMessage: (successMessage: string | null) => {},
})

function Auth ({ state, appearance, logo, socialLayout = 'horizontal', additionalSignupFields }: {
    state: State;
} & CustomizationOptions & {
  additionalSignupFields?: AdditionalSignupFields;
}) {
  const [errorMessage, setErrorMessage] = useState<ErrorMessage | null>(null);
  const [successMessage, setSuccessMessage] = useState<string | null>(null);
  const [isLoading, setIsLoading] = useState(false);

  const customStyle = useMemo(() => ({
    ...tokenObjToCSSVars('color', appearance?.colors ?? {}),
    ...tokenObjToCSSVars('font-size', appearance?.fontSizes ?? {}),
  }), [appearance])

  const titles: Record<State, string> = {
    login: 'Log in to your account',
    signup: 'Create a new account',
    "forgot-password": "Forgot your password?",
    "reset-password": "Reset your password",
    "verify-email": "Email verification",
  }
  const title = titles[state]

  const socialButtonsDirection = socialLayout === 'vertical' ? 'vertical' : 'horizontal'

  return (
    <div className={styles.container} style={customStyle}>
      <div>
        {logo && (<img style={logoStyle} src={logo} alt='Your Company' />)}
        <h2 className={styles.headerText}>{title}</h2>
      </div>

      {errorMessage && (
        <MessageError>
          {errorMessage.title}{errorMessage.description && ': '}{errorMessage.description}
        </MessageError>
      )}
      {successMessage && <MessageSuccess>{successMessage}</MessageSuccess>}
      <AuthContext.Provider value={{ isLoading, setIsLoading, setErrorMessage, setSuccessMessage }}>
        {(state === 'login' || state === 'signup') && (
          <LoginSignupForm
            state={state}
            socialButtonsDirection={socialButtonsDirection}
            additionalSignupFields={additionalSignupFields}
          />
        )}
        {state === 'forgot-password' && (<ForgotPasswordForm />)}
        {state === 'reset-password' && (<ResetPasswordForm />)}
        {state === 'verify-email' && (<VerifyEmailForm />)}
      </AuthContext.Provider>
    </div>
  )
}

// PRIVATE API
export default Auth;
