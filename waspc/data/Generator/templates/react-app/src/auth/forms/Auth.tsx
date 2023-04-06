{{={= =}=}}
import { useState, FormEvent } from 'react'
import { useHistory } from 'react-router-dom'
import { createTheme } from '@stitches/react'

{=# isUsernameAndPasswordAuthEnabled =}
import signup from '../signup.js'
import login from '../login.js'
{=/ isUsernameAndPasswordAuthEnabled =}
{=# isEmailAuthEnabled =}
import { signup } from '../email/actions/signup.js'
import { login } from '../email/actions/login.js'
{=/ isEmailAuthEnabled =}
{=# isExternalAuthEnabled =}
import * as SocialIcons from './SocialIcons'
import { SocialButton } from './SocialButton';
{=/ isExternalAuthEnabled =}

import config from '../../config.js'
import { styled } from '../../stitches.config'

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
})

const ErrorMessage = styled(Message, {
  background: '$errorBackground',
  color: '$errorText',
})

const SuccessMessage = styled(Message, {
  background: '$successBackground',
  color: '$successText',
})

{=# isGoogleAuthEnabled =}
const googleSignInUrl = `${config.apiUrl}{= googleSignInPath =}`
{=/ isGoogleAuthEnabled =}
{=# isGitHubAuthEnabled =}
const gitHubSignInUrl = `${config.apiUrl}{= gitHubSignInPath =}`
{=/ isGitHubAuthEnabled =}

function Auth (
  { isLogin, appearance, logo, socialLayout }: {
    isLogin: boolean;
    logo: string; 
    socialLayout: "horizontal" | "vertical";
    appearance: Parameters<typeof createTheme>[0];
  },
) {
  const [errorMessage, setErrorMessage] = useState<string | null>(null);
  const [successMessage, setSuccessMessage] = useState<string | null>(null);
  const [isLoading, setIsLoading] = useState(false);
  {=# isAnyPasswordBasedAuthEnabled =}
  const history = useHistory();
  const onErrorHandler = (error) => {
    setErrorMessage(error.message)
  };
  {=/ isAnyPasswordBasedAuthEnabled =}
  {=# isUsernameAndPasswordAuthEnabled =}
  const { handleSubmit, usernameFieldVal, passwordFieldVal, setUsernameFieldVal, setPasswordFieldVal } = useUsernameAndPassword({
    isLogin,
    onError: onErrorHandler,
    onSuccess() {
      // Redirect to configured page, defaults to /.
      history.push('{= onAuthSucceededRedirectTo =}')
    },
  });
  {=/ isUsernameAndPasswordAuthEnabled =}
  {=# isEmailAuthEnabled =}
  const { handleSubmit, emailFieldVal, passwordFieldVal, setEmailFieldVal, setPasswordFieldVal } = useEmail({
    isLogin,
    onError: onErrorHandler,
    showEmailVerificationPending() {
      setSuccessMessage('Check your email for a confirmation link.')
    },
    onLoginSuccess() {
      // Redirect to configured page, defaults to /.
      history.push('{= onAuthSucceededRedirectTo =}')
    }
  });
  {=/ isEmailAuthEnabled =}
  {=# isAnyPasswordBasedAuthEnabled =}
  async function onSubmit (event: FormEvent<HTMLFormElement>) {
    event.preventDefault();
    setIsLoading(true);
    setErrorMessage(null);
    setSuccessMessage(null);
    try {
      await handleSubmit();
    } finally {
      setIsLoading(false);
    }
  }
  {=/ isAnyPasswordBasedAuthEnabled =}

  // TODO(matija): this is called on every render, is it a problem?
  // If we do it in useEffect(), then there is a glitch between the default color and the
  // user provided one.
  const customTheme = createTheme(appearance)

  const cta = isLogin ? 'Log in' : 'Sign up'
  const title = isLogin ? 'Log in to your account' : 'Create a new account'

  const socialButtonsDirection = socialLayout === 'vertical' ? 'vertical' : 'horizontal'

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

      {=# isExternalAuthEnabled =}
        <SocialAuth>
          <SocialAuthLabel>{cta} with</SocialAuthLabel>
          <SocialAuthButtons gap='large' direction={socialButtonsDirection}>
            {=# isGoogleAuthEnabled =}
              <SocialButton href={googleSignInUrl}><SocialIcons.Google/></SocialButton>
            {=/ isGoogleAuthEnabled =}

            {=# isGitHubAuthEnabled =}
              <SocialButton href={gitHubSignInUrl}><SocialIcons.GitHub/></SocialButton>
            {=/ isGitHubAuthEnabled =}
          </SocialAuthButtons>
        </SocialAuth>
      {=/ isExternalAuthEnabled =}
      
      {=# areBothSocialAndPasswordBasedAuthEnabled =}
        <OrContinueWith>
          <OrContinueWithLineContainer>
            <OrContinueWithLine/>
          </OrContinueWithLineContainer>
          <OrContinueWithTextContainer>
            <OrContinueWithText>Or continue with</OrContinueWithText>
          </OrContinueWithTextContainer>
        </OrContinueWith>
      {=/ areBothSocialAndPasswordBasedAuthEnabled =}
      {=# isAnyPasswordBasedAuthEnabled =}
        <UserPassForm onSubmit={onSubmit}>
          {=# isUsernameAndPasswordAuthEnabled =}
          <FormItemGroup>
            <FormLabel>Username</FormLabel>
            <FormInput
              type="text"
              required
              value={usernameFieldVal}
              onChange={e => setUsernameFieldVal(e.target.value)}
              disabled={isLoading}
            />
          </FormItemGroup>
          {=/ isUsernameAndPasswordAuthEnabled =}
          {=# isEmailAuthEnabled =}
          <FormItemGroup>
            <FormLabel>E-mail</FormLabel>
            <FormInput
              type="email"
              required
              value={emailFieldVal}
              onChange={e => setEmailFieldVal(e.target.value)}
              disabled={isLoading}
            />
          </FormItemGroup>
          {=/ isEmailAuthEnabled =}
          <FormItemGroup>
            <FormLabel>Password</FormLabel>
            <FormInput
              type="password"
              required
              value={passwordFieldVal}
              onChange={e => setPasswordFieldVal(e.target.value)}
              disabled={isLoading}
            />
          </FormItemGroup>

          <FormItemGroup>
            <SubmitButton type="submit" disabled={isLoading}>{cta}</SubmitButton>
          </FormItemGroup>
        </UserPassForm>
      {=/ isAnyPasswordBasedAuthEnabled =}
    </Container>
  )
}

export default Auth;

{=# isUsernameAndPasswordAuthEnabled =}
function useUsernameAndPassword({
  onError,
  onSuccess,
  isLogin,
}: {
  onError: (error: Error) => void;
  onSuccess: () => void;
  isLogin: boolean;
}) {
  const [usernameFieldVal, setUsernameFieldVal] = useState('')
  const [passwordFieldVal, setPasswordFieldVal] = useState('')

  async function handleSubmit (event: FormEvent<HTMLFormElement>) {
    try {
      if (!isLogin) {
        await signup({ username: usernameFieldVal, password: passwordFieldVal })
      }
      await login (usernameFieldVal, passwordFieldVal)

      setUsernameFieldVal('')
      setPasswordFieldVal('')
      onSuccess()
    } catch (err: unknown) {
      onError(err as Error)
    }
  }

  return { handleSubmit, usernameFieldVal, passwordFieldVal, setUsernameFieldVal, setPasswordFieldVal }
}
{=/ isUsernameAndPasswordAuthEnabled =}
{=# isEmailAuthEnabled =}
function useEmail({
  onError,
  showEmailVerificationPending,
  onLoginSuccess,
  isLogin,
}: {
  onError: (error: Error) => void;
  showEmailVerificationPending: () => void;
  onLoginSuccess: () => void;
  isLogin: boolean;
}) {
  const [emailFieldVal, setEmailFieldVal] = useState('')
  const [passwordFieldVal, setPasswordFieldVal] = useState('')

  async function handleSubmit (event: FormEvent<HTMLFormElement>) {
    try {
      if (isLogin) {
        await login({ email: emailFieldVal, password: passwordFieldVal })
        onLoginSuccess()
      } else {
        await signup({ email: emailFieldVal, password: passwordFieldVal })
        {=# isEmailVerificationRequired =}
        showEmailVerificationPending()
        {=/ isEmailVerificationRequired =}
        {=^ isEmailVerificationRequired =}
        await login ({ email: emailFieldVal, password: passwordFieldVal})
        onLoginSuccess()
        {=/ isEmailVerificationRequired =}
      }

      setEmailFieldVal('')
      setPasswordFieldVal('')
    } catch (err: unknown) {
      onError(err as Error)
    }
  }

  return { handleSubmit, emailFieldVal, passwordFieldVal, setEmailFieldVal, setPasswordFieldVal }
}
{=/ isEmailAuthEnabled =}