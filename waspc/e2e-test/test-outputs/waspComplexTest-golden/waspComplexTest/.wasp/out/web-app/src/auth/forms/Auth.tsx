import { useState, FormEvent, useEffect, useCallback } from 'react'
import { useHistory, useLocation } from 'react-router-dom'
import { createTheme } from '@stitches/react'

import * as SocialIcons from './SocialIcons'
import { SocialButton } from './SocialButton';

import config from '../../config.js'
import { styled } from '../../stitches.config'
import { State, CustomizationOptions } from './types'

const logoStyle = {
  height: '3rem'
}

const Container = styled('div', {
  display: 'flex',
  flexDirection: 'column',
})

const HeaderText = styled('h2', {
  fontSize: '1.875rem',
  fontWeight: '700',
  marginTop: '1.5rem'
})

const SocialAuth = styled('div', {
  marginTop: '1.5rem'
})

const SocialAuthLabel = styled('div', {
  fontWeight: '500',
  fontSize: '$sm'
})

const SocialAuthButtons = styled('div', {
  marginTop: '0.5rem',
  display: 'flex',

  variants: {
    direction: {
      horizontal: {
        display: 'grid',
        gridTemplateColumns: 'repeat(auto-fit, minmax(48px, 1fr))',
      },
      vertical: {
        flexDirection: 'column',
        margin: '8px 0',
      }
    },
    gap: {
      small: {
        gap: '4px',
      },
      medium: {
        gap: '8px',
      },
      large: {
        gap: '16px',
      }
    }
  }
})

const OrContinueWith = styled('div', {
  position: 'relative',
  marginTop: '1.5rem'
})

const OrContinueWithLineContainer = styled('div', {
  position: 'absolute',
  inset: '0px',
  display: 'flex',
  alignItems: 'center'
})

const OrContinueWithLine = styled('div', {
  width: '100%',
  borderTopWidth: '1px',
  borderColor: '$gray500'
})

const OrContinueWithTextContainer = styled('div', {
  position: 'relative',
  display: 'flex',
  justifyContent: 'center',
  fontSize: '$sm'
})

const OrContinueWithText = styled('span', {
  backgroundColor: 'white',
  paddingLeft: '0.5rem',
  paddingRight: '0.5rem'
})

// Email/username form

const UserPassForm = styled('form', {
  marginTop: '1.5rem'
})

const FormItemGroup = styled('div', {
  '& + div': {
    marginTop: '1.5rem'
  }
})

const FormLabel = styled('label', {
  display: 'block',
  fontSize: '$sm',
  fontWeight: '500'
})

const FormInput = styled('input', {
  display: 'block',
  lineHeight: '1.5rem',
  fontSize: '$sm',
  borderWidth: '1px',
  borderColor: '$gray600',
  backgroundColor: '#f8f4ff',
  boxShadow: '0 1px 2px 0 rgba(0, 0, 0, 0.05)',
  '&:focus': {
    borderWidth: '1px',
    borderColor: '$gray700',
    boxShadow: '0 1px 2px 0 rgba(0, 0, 0, 0.05)',
  },
  '&:disabled': {
    opacity: 0.5,
    cursor: 'not-allowed',
    backgroundColor: '$gray400',
    borderColor: '$gray400',
    color: '$gray500',
  },

  borderRadius: '0.375rem',
  width: '100%',

  paddingTop: '0.375rem',
  paddingBottom: '0.375rem',

  marginTop: '0.5rem'
})

const SubmitButton = styled('button', {
  display: 'flex',
  justifyContent: 'center',

  width: '100%',
  borderWidth: '1px',
  borderColor: '$brand',
  backgroundColor: '$brand',
  color: '$submitButtonText',

  padding: '0.5rem 0.75rem',
  boxShadow: '0 1px 2px 0 rgba(0, 0, 0, 0.05)',

  fontWeight: '600',
  fontSize: '$sm',
  lineHeight: '1.25rem',
  borderRadius: '0.375rem',

  // TODO(matija): extract this into separate BaseButton component and then inherit it.
  '&:hover': {
    backgroundColor: '$brandAccent',
    borderColor: '$brandAccent',
  },
  '&:disabled': {
    opacity: 0.5,
    cursor: 'not-allowed',
    backgroundColor: '$gray400',
    borderColor: '$gray400',
    color: '$gray500',
  },
  transitionTimingFunction: 'cubic-bezier(0.4, 0, 0.2, 1)',
  transitionDuration: '100ms'
})

const Message = styled('div', {
  padding: '0.5rem 0.75rem',
  borderRadius: '0.375rem',
  marginTop: '1rem',
  background: '$gray400',
})

const ErrorMessage = styled(Message, {
  background: '$errorBackground',
  color: '$errorText',
})

const SuccessMessage = styled(Message, {
  background: '$successBackground',
  color: '$successText',
})

const googleSignInUrl = `${config.apiUrl}/auth/google/login`

function Auth ({ state, appearance, logo, socialLayout = 'horizontal' }: {
    state: State;
} & CustomizationOptions) {
  const isLogin = state === "login";
  const [errorMessage, setErrorMessage] = useState<string | null>(null);
  const [successMessage, setSuccessMessage] = useState<string | null>(null);
  const [isLoading, setIsLoading] = useState(false);

  // TODO(matija): this is called on every render, is it a problem?
  // If we do it in useEffect(), then there is a glitch between the default color and the
  // user provided one.
  const customTheme = createTheme(appearance)

  const cta = isLogin ? 'Log in' : 'Sign up'
  const titles: Record<State, string> = {
    login: 'Log in to your account',
    signup: 'Create a new account',
    "forgot-password": "Forgot your password?",
    "reset-password": "Reset your password",
    "verify-email": "Email verification",
  }
  const title = titles[state]

  const socialButtonsDirection = socialLayout === 'vertical' ? 'vertical' : 'horizontal'

  const loginSignupForm = (<>
        <SocialAuth>
          <SocialAuthLabel>{cta} with</SocialAuthLabel>
          <SocialAuthButtons gap='large' direction={socialButtonsDirection}>
              <SocialButton href={googleSignInUrl}><SocialIcons.Google/></SocialButton>

          </SocialAuthButtons>
        </SocialAuth>
      
  </>)

  return (
    <Container className={customTheme}>
      <div>
        {logo && (
          <img style={logoStyle} src={logo} alt='Your Company' />
        )}
        <HeaderText>{title}</HeaderText>
      </div>

      {errorMessage && <ErrorMessage>{errorMessage}</ErrorMessage>}
      {successMessage && <SuccessMessage>{successMessage}</SuccessMessage>}
      {(state === 'login' || state === 'signup') && loginSignupForm}
    </Container>
  )
}

export default Auth;


